{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.QBE where

import Data.Text (Text)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import Data.ByteString (ByteString)
import Data.Word (Word64)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Maybe (maybeToList)
import Prettyprinter
  ( Pretty(pretty), Doc, (<+>), vsep, hsep, hang, punctuate, group, flatAlt
  , space, encloseSep, tupled, comma, equals, braces )
-- Instances
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)
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
  deriving (Show, Eq, Ord, IsString, NFData, Hashable)

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
  -- MAYBE just use a signed type
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

data TypeDef
  = TypeDef (Ident 'AggregateTy) (Maybe Alignment) [(SubTy, Maybe Amount)]
  | Opaque (Ident 'AggregateTy) Alignment Size
  deriving (Show, Eq)

instance Pretty TypeDef where
  pretty (TypeDef ident alignment def) =
    "type" <+> pretty ident <+> equals
    <> maybe mempty (\x -> space <> pretty x) alignment
    <+> braced (prettyItem <$> def)
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

instance Pretty DataDef where
  pretty (DataDef linkage ident alignment fields) = vsep
    [ vsep $ pretty <$> linkage
    , hsep $ ("data" <+> pretty ident <+> equals)
           : maybeToList (("align" <+>) . pretty <$> alignment)
    , braced (pretty <$> fields)
    ]

data DataItem
  = Symbol (Ident 'Global) (Maybe Alignment)
  | String ByteString
  | Const Const
  deriving (Show, Eq)

instance Pretty DataItem where
  pretty (Symbol ident alignment) =
    hsep $ pretty ident : maybeToList ((pretty '+' <+>) . pretty <$> alignment)
  pretty (String bs) = pretty $ show bs -- HACK: hoping that the escape sequences are the same...
  pretty (Const c) = pretty c

data Field
  = FieldExtTy ExtTy (NonEmpty DataItem)
  | FieldZero Size
  deriving (Show, Eq)

instance Pretty Field where
  pretty (FieldExtTy extTy items) = pretty extTy <+> hsep (toList $ pretty <$> items)
  pretty (FieldZero size) = pretty 'z' <+> pretty size

-- ** Functions
---------------

-- | Function definition. The 'Maybe (Ident \'Temporary)' is the environment
data FuncDef = FuncDef [Linkage] (Maybe AbiTy) (Ident 'Global) (Maybe (Ident 'Temporary)) [Param] Variadic (NonEmpty Block)
  deriving (Show, Eq)

instance Pretty FuncDef where
  pretty (FuncDef linkage abiTy ident env params variadic blocks) = vsep
    [ vsep $ pretty <$> linkage
    , "function" <+> pretty abiTy <+> pretty ident
    , tupled $
        maybeToList (("env" <+>) . pretty <$> env)
        ++ fmap pretty params
        ++ maybeToList (prettyVariadic variadic)
    , braces $ vsep $ toList $ pretty <$> blocks
    ]

data AbiTy = AbiBaseTy BaseTy | AbiAggregateTy (Ident 'AggregateTy)
  deriving (Show, Eq)

instance Pretty AbiTy where
  pretty (AbiBaseTy baseTy) = pretty baseTy
  pretty (AbiAggregateTy ident) = pretty ident

data Param = Param AbiTy (Ident 'Temporary)
  deriving (Show, Eq)

instance Pretty Param where
  pretty (Param abiTy ident) = pretty abiTy <+> pretty ident

data Variadic = Variadic | NoVariadic
  deriving (Show, Eq)

prettyVariadic :: Variadic -> Maybe (Doc a)
prettyVariadic Variadic = Just "..."
prettyVariadic NoVariadic = Nothing

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

instance Pretty Block where
  pretty (Block ident phis insts jump) = hang 4 $ vsep $ concat
    [ [pretty ident]
    , pretty <$> phis
    , pretty <$> insts
    , [pretty jump]
    ]

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

data Phi = Phi Assignment [PhiArg]
  deriving (Show, Eq)

instance Pretty Phi where
  pretty (Phi assignment args) =
    pretty assignment <+> "phi" <+> hsep (punctuate comma $ pretty <$> args)

data PhiArg = PhiArg (Ident 'Label) Val
  deriving (Show, Eq)

instance Pretty PhiArg where
  pretty (PhiArg label val) = pretty label <+> pretty val

data Inst
  -- Arithmetic and Bits
  = BinaryOp Assignment BinaryOp Val Val
  | Neg Assignment Val
  -- Memory
  | Store ExtTy Val Val
  -- MAYBE collapse all the Loads in a single Load constructor and just discard
  -- the intrepr when unused.
  | Load Assignment BaseTy Val -- ^ @\<ident\> =\<baseTy\> load\<baseTy\> \<val\>@
  | LoadW Assignment IntRepr Val -- ^ @\<ident\> =\<baseTy\> load\<intRepr\>w \<val\>@
  | LoadH Assignment IntRepr Val
  | LoadB Assignment IntRepr Val
  -- Comparisons
  | Compare Assignment Comparison BaseTy Val Val
  -- Conversions
  -- | @extsw@/@extuw@
  | ExtW Assignment IntRepr Val
  -- | @extsh@/@extuh@
  | ExtH Assignment IntRepr Val
  -- | @extsb@/@extub@
  | ExtB Assignment IntRepr Val
  -- | @exts@. There is only one possible instruction type, so there's
  -- only an 'Ident' instead of a full 'Assignment'
  | ExtS (Ident 'Temporary) Val
  -- | @truncd@. There is only one possible instruction type, so there's
  -- only an 'Ident' instead of a full 'Assignment'
  | TruncD (Ident 'Temporary) Val
  -- | @stosi@/@stoui@
  | StoI Assignment IntRepr Val
  -- | @dtosi@/@dtoui@
  | DtoI Assignment IntRepr Val
  -- | @swtof@/@uwtof@
  | WtoF Assignment IntRepr Val
  -- | @sltof@/@ultof@
  | LtoF Assignment IntRepr Val
  -- Cast and Copy
  | Cast Assignment Val
  | Copy Assignment Val
  -- Calls
  -- | the fields are: assignment, function name, environment, arguments, variadic arguments
  | Call (Maybe (Ident 'Temporary, AbiTy)) Val (Maybe Val) [Arg] [Arg]
  -- Variadic
  | VaStart (Ident 'Temporary)
  | VaArg Assignment (Ident 'Temporary)
  deriving (Show, Eq)

instance Pretty Inst where
  pretty (BinaryOp assignment op v1 v2) =
    pretty assignment <+> pretty op <+> pretty v1 <> comma <+> pretty v2
  pretty (Neg assignment v) =
    pretty assignment <+> "neg" <+> pretty v
  pretty (Store ty v address) =
    "store" <> pretty ty <+> pretty v <> comma <+> pretty address
  pretty (Load  assignment loadTy addr) =
    pretty assignment <+> "load" <> pretty loadTy <+> pretty addr
  pretty (LoadW assignment intRepr addr) =
    pretty assignment <+> "load" <> pretty intRepr <> pretty 'w' <+> pretty addr
  pretty (LoadH assignment intRepr addr) =
    pretty assignment <+> "load" <> pretty intRepr <> pretty 'h' <+> pretty addr
  pretty (LoadB assignment intRepr addr) =
    pretty assignment <+> "load" <> pretty intRepr <> pretty 'b' <+> pretty addr
  pretty (Compare assignment comp compTy v1 v2) =
    pretty assignment <+> pretty 'c' <> pretty comp <> pretty compTy <+> pretty v1 <> comma <+> pretty v2
  pretty (ExtW assignment intRepr v) =
    pretty assignment <+> "ext" <> pretty intRepr <> pretty 'w' <+> pretty v
  pretty (ExtH assignment intRepr v) =
    pretty assignment <+> "ext" <> pretty intRepr <> pretty 'h' <+> pretty v
  pretty (ExtB assignment intRepr v) =
    pretty assignment <+> "ext" <> pretty intRepr <> pretty 'b' <+> pretty v
  pretty (ExtS res v) = pretty res <+> equals <> pretty 'd' <+> "exts" <+> pretty v
  pretty (TruncD res v) = pretty res <+> equals <> pretty 's' <+> "truncd" <+> pretty v
  pretty (StoI assignment intRepr v) = pretty assignment <+> "sto" <> pretty intRepr <> pretty 'i' <+> pretty v
  pretty (DtoI assignment intRepr v) = pretty assignment <+> "dto" <> pretty intRepr <> pretty 'i' <+> pretty v
  pretty (WtoF assignment intRepr v) = pretty assignment <+> pretty intRepr <> "wtof" <+> pretty v
  pretty (LtoF assignment intRepr v) = pretty assignment <+> pretty intRepr <> "ltof" <+> pretty v
  pretty (Cast assignment v) = pretty assignment <+> "cast" <+> pretty v
  pretty (Copy assignment v) = pretty assignment <+> "copy" <+> pretty v
  pretty (Call assignment func env args variadics) = hsep $
    maybeToList (prettyAssignment <$> assignment) ++
    [ "call"
    , pretty func
    , tupled $ maybeToList (("env" <+>) . pretty <$> env)
            ++ fmap pretty args
            ++ variadics'
    ]
    where
      prettyAssignment (ident, ty) = pretty ident <+> equals <> pretty ty
      variadics' = if null variadics then [] else "..." : fmap pretty variadics
  pretty (VaStart argList) = "vastart" <+> pretty argList
  pretty (VaArg assignment argList) = pretty assignment <+> "vaarg" <+> pretty argList

data Assignment = Assignment (Ident 'Temporary) BaseTy
  deriving (Show, Eq)

-- | Infix synonym of 'Assignment'
pattern (:=) :: Ident 'Temporary -> BaseTy -> Assignment
pattern (:=) ident ty = Assignment ident ty

instance Pretty Assignment where
  pretty (Assignment ident ty) = pretty ident <+> equals <> pretty ty

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

instance Pretty BinaryOp where
  pretty Add            = "add"
  pretty Sub            = "sub"
  pretty (Div Signed)   = "div"
  pretty (Div Unsigned) = "udiv"
  pretty Mul            = "mul"
  pretty (Rem Signed)   = "rem"
  pretty (Rem Unsigned) = "rem"
  pretty Or             = "or"
  pretty Xor            = "xor"
  pretty And            = "and"
  pretty Sar            = "sar"
  pretty Shr            = "shr"
  pretty Shl            = "shl"

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

instance Pretty Comparison where
  pretty Eq = "eq"
  pretty Ne = "ne"
  pretty (Le intRepr) = pretty intRepr <> "le"
  pretty (Lt intRepr) = pretty intRepr <> "lt"
  pretty (Ge intRepr) = pretty intRepr <> "ge"
  pretty (Gt intRepr) = pretty intRepr <> "gt"
  pretty O = "o"
  pretty Uo = "uo"

instance Pretty IntRepr where
  pretty Signed = pretty 's'
  pretty Unsigned = pretty 'u'

data Arg = Arg AbiTy Val
  deriving (Show, Eq)

instance Pretty Arg where
  pretty (Arg abiTy val) = pretty abiTy <+> pretty val

-- * Program
------------

data Program = Program [TypeDef] [DataDef] [FuncDef]
  deriving (Show, Eq)

instance Pretty Program where
  pretty (Program typeDefs dataDefs funcDefs) = vsep $ concat
    [ pretty <$> typeDefs
    , pretty <$> dataDefs
    , pretty <$> funcDefs
    ]

-- * Utilities
--------------

-- like 'list' and 'tupled'
braced :: [Doc ann] -> Doc ann
braced = group . encloseSep (flatAlt "{ " "{")
                            (flatAlt " }" "}")
                            ", "
