{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QBE where

import Data.Text (Text)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import Data.ByteString (ByteString)
import Data.Word (Word64)
import Data.List.NonEmpty (NonEmpty)
import Prettyprinter
  ( Pretty(pretty), (<+>)
  , space, encloseSep, lbrace, rbrace, comma, equals, braces )
-- Instances
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.String (IsString)

-- * Identifiers
----------------

type RawIdent = ShortText

data Sigil
  = AggregateTy -- ^ @:@
  | Global -- ^ @$@
  | Temporary -- ^ @%@
  | Label -- ^ @\@@
  deriving (Show, Eq)

-- | QBE identifiers. The sigil is represented at the type level, so that
-- mixing incompatible identifiers is impossible.
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> pretty $ Jmp $ Ident @'Label "a"
-- jmp @a
-- >>> pretty $ Jmp $ Ident @'Global "a"
-- <interactive>:5:16: error:
--     • Couldn't match type ‘'Global’ with ‘'Label’
--       Expected: Ident 'Label
--         Actual: Ident 'Global
--     • In the second argument of ‘($)’, namely ‘Ident @'Global "a"’
--       In the second argument of ‘($)’, namely ‘Jmp $ Ident @'Global "a"’
--       In the expression: pretty $ Jmp $ Ident @'Global "a"
newtype Ident (t :: Sigil) = Ident RawIdent
  deriving (Show, Eq, Ord, IsString, Binary, NFData, Hashable)

instance Pretty (Ident 'AggregateTy) where
  pretty (Ident raw) = pretty ':' <> pretty (TS.toText raw)
instance Pretty (Ident 'Global) where
  pretty (Ident raw) = pretty '$' <> pretty (TS.toText raw)
instance Pretty (Ident 'Temporary) where
  pretty (Ident raw) = pretty '%' <> pretty (TS.toText raw)
instance Pretty (Ident 'Label) where
  pretty (Ident raw) = pretty '@' <> pretty (TS.toText raw)

-- * Types
----------

data BaseTy = Word | Long | Single | Double
  deriving (Show, Eq)

instance Pretty BaseTy where
  pretty Word   = pretty 'w'
  pretty Long   = pretty 'l'
  pretty Single = pretty 's'
  pretty Double = pretty 'd'

data ExtTy = BaseTy BaseTy | Byte | HalfWord
  deriving (Show, Eq)

instance Pretty ExtTy where
  pretty (BaseTy baseTy) = pretty baseTy
  pretty Byte = pretty 'b'
  pretty HalfWord = pretty 'h'

-- * Constants
--------------

data Const
  = CInt Bool Word64 -- ^ The 'Bool' is whether to negate
  | CSingle Float
  | CDouble Double
  | CGlobal (Ident 'Global)
  deriving (Show, Eq)

instance Pretty Const where
  pretty (CInt negative int) | negative = pretty '-' <> pretty int
                             | otherwise = pretty int
  pretty (CSingle float) = "s_" <> pretty float
  pretty (CDouble double) = "d_" <> pretty double
  pretty (CGlobal ident) = pretty ident

-- * Linkage
------------

data Linkage
  = Export
  | Section ShortText (Maybe Text)
  deriving (Show, Eq)

instance Pretty Linkage where
  pretty Export = "export"
  pretty (Section secName Nothing) = "section" <+> pretty (TS.toText secName)
  pretty (Section secName (Just secFlags)) =
    "section" <+> pretty (TS.toText secName) <+> pretty secFlags

-- * Definitions
----------------

type Alignment = Word64
type Size = Word64
type Amount = Word64

-- ** Aggregate types
---------------------

data Typedef
  = Typedef (Ident 'AggregateTy) (Maybe Alignment) [(SubTy, Maybe Amount)]
  | Opaque (Ident 'AggregateTy) Alignment Size
  deriving (Show, Eq)

instance Pretty Typedef where
  pretty (Typedef ident alignment def) =
    "type" <+> pretty ident <+> equals
    <> maybe mempty (\x -> space <> pretty x) alignment
    <+> encloseSep lbrace rbrace (comma <> space) (prettyItem <$> def)
    where
      prettyItem (subTy, Nothing    ) = pretty subTy
      prettyItem (subTy, Just amount) = pretty subTy <+> pretty amount
  pretty (Opaque ident alignment size) =
    "type" <+> pretty ident <+> equals
    <+> "align" <+> pretty alignment <+> braces (pretty size)

data SubTy
  = SubExtTy ExtTy
  | SubAggregateTy (Ident 'AggregateTy)
  deriving (Show, Eq)

instance Pretty SubTy where
  pretty (SubExtTy extTy) = pretty extTy
  pretty (SubAggregateTy ident) = pretty ident

-- ** Data
----------

data DataDef = DataDef [Linkage] (Ident 'Global) (Maybe Alignment) [Field]
  deriving (Show, Eq)

data DataItem
  = Symbol (Ident 'Global) Alignment
  | String ByteString
  | Const Const
  deriving (Show, Eq)

data Field
  = FieldExtTy ExtTy (NonEmpty DataItem)
  | FieldZero Size
  deriving (Show, Eq)

-- ** Functions
---------------

-- | Function definition. The 'Maybe (Ident \'Temporary)' is the environment
data FuncDef = FuncDef [Linkage] (Maybe AbiTy) (Ident 'Global) (Maybe (Ident 'Temporary)) [Param] Variadic (NonEmpty Block)
  deriving (Show, Eq)

data AbiTy = AbiBaseTy BaseTy | AbiAggregateTy (Ident 'AggregateTy)
  deriving (Show, Eq)

data Param = Param AbiTy (Ident 'Temporary)
  deriving (Show, Eq)

data Variadic = Variadic | NoVariadic
  deriving (Show, Eq)

-- * Control
------------

data Val
  = ValConst Const
  | ValTemporary (Ident 'Temporary)
  | ValGlobal (Ident 'Global)
  deriving (Show, Eq)

instance Pretty Val where
  pretty (ValConst c) = pretty c
  pretty (ValTemporary ident) = pretty ident
  pretty (ValGlobal ident) = pretty ident

data Block = Block (Ident 'Label) [Phi] [Inst] Jump
  deriving (Show, Eq)

data Jump
  = Jmp (Ident 'Label)
  | Jnz Val (Ident 'Label) (Ident 'Label)
  | Ret (Maybe Val)
  deriving (Show, Eq)

instance Pretty Jump where
  pretty (Jmp ident) = "jmp" <+> pretty ident
  pretty (Jnz val label1 label2) =
    "jnz" <+> pretty val <> comma
    <+> pretty label1 <> comma
    <+> pretty label2
  pretty (Ret val) = "ret" <+> pretty val

-- * Instructions
-----------------

data Phi = Phi (Ident 'Temporary) BaseTy [(Ident 'Label, Val)]
  deriving (Show, Eq)

data Inst
  -- Arithmetic and Bits
  = BinaryOp (Ident 'Temporary) BaseTy BinaryOp Val Val
  | Neg (Ident 'Temporary) BaseTy Val
  -- Memory
  | Store ExtTy Val Val
  | Load (Ident 'Temporary) BaseTy BaseTy Val -- ^ @\<ident\> =\<baseTy\> load\<baseTy\> \<val\>@
  | LoadW (Ident 'Temporary) BaseTy IntRepr Val -- ^ @\<ident\> =\<baseTy\> load\<intRepr\>w \<val\>@
  | LoadH (Ident 'Temporary) BaseTy IntRepr Val
  | LoadB (Ident 'Temporary) BaseTy IntRepr Val
  -- Comparisons
  | Compare (Ident 'Temporary) BaseTy Comparison BaseTy Val Val
  -- Conversions
  -- | @extsw@/@extuw@. There is only one possible instruction type, so there's
  -- no 'BaseTy' argument
  | ExtW (Ident 'Temporary)        IntRepr Val
  -- | @extsh@/@extuh@
  | ExtH (Ident 'Temporary) BaseTy IntRepr Val
  -- | @extsb@/@extub@
  | ExtB (Ident 'Temporary) BaseTy IntRepr Val
  -- | @exts@. There is only one possible instruction type, so there's
  -- no 'BaseTy' argument
  | Exts (Ident 'Temporary) Val
  -- | @truncd@. There is only one possible instruction type, so there's
  -- no 'BaseTy' argument
  | Truncd (Ident 'Temporary) Val
  -- | @stosi@/@stoui@
  | StoI (Ident 'Temporary) BaseTy IntRepr Val
  -- | @dtosi@/@dtoui@
  | DtoI (Ident 'Temporary) BaseTy IntRepr Val
  -- | @swtof@/@uwtof@
  | WtoF (Ident 'Temporary) BaseTy IntRepr Val
  -- | @sltof@/@ultof@
  | LtoF (Ident 'Temporary) BaseTy IntRepr Val
  -- Cast and Copy
  | Cast (Ident 'Temporary) BaseTy Val
  | Copy (Ident 'Temporary) BaseTy Val
  -- Calls
  -- | the fields are: assignment, function name, environment, arguments, variadic arguments
  | Call (Maybe (Ident 'Temporary, AbiTy)) Val (Maybe Val) [Arg] [Arg]
  -- Variadic
  | VaStart (Ident 'Temporary)
  | VaArg (Ident 'Temporary) BaseTy (Ident 'Temporary)
  deriving (Show, Eq)

data IntRepr = Signed | Unsigned
  deriving (Show, Eq)

data BinaryOp
  -- | @add@
  = Add
  -- | @sub@
  | Sub
  -- | @div@/@udiv@. @Div Signed@ gets translated to @div@, so it will work
  -- also on floats
  | Div IntRepr
  -- | @mul@
  | Mul
  -- | @rem@/@urem@
  | Rem IntRepr
  -- | @or@
  | Or
  -- | @xor@
  | Xor
  -- | @and@
  | And
  -- | @sar@
  | Sar
  -- | @shr@
  | Shr
  -- | @shl@
  | Shl
  deriving (Show, Eq)

-- | Comparison operators.
-- Where there's a @'Maybe' 'IntRepr'@, 'Nothing' means floating point
-- (@le@, @lt@, @ge@, @gt@), while @'Just' r@ means integer
-- (@sle@, @ule@, @slt@, @ult@...)
data Comparison
  -- Universal comparison
  = Eq -- ^ equality
  | Ne -- ^ inequality
  | Le (Maybe IntRepr) -- ^ lower or equal
  | Lt (Maybe IntRepr) -- ^ lower
  | Ge (Maybe IntRepr) -- ^ greater or equal
  | Gt (Maybe IntRepr) -- ^ greater
  -- Floating point only comparison
  | O -- ^ ordered (no operand is a NaN) (floating point only)
  | Uo -- ^ unordered (at least one operand is a NaN) (floating point only)
  deriving (Show, Eq)

data Arg = Arg AbiTy Val
  deriving (Show, Eq)
