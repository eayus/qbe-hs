{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Language.QBE
Description : Types and Pretty instances for the QBE IL
Copyright   : (c) Francesco Gazzetta, 2022
License     : BSD-3-Clause
Maintainer  : Francesco Gazzetta <fgaz@fgaz.me>

This module contains datatypes representing the various structures of
the [intermediate language](https://c9x.me/compile/doc/il.html)
of the [QBE](https://c9x.me/compile/) compiler backend.

All datatypes also have 'Pretty' instances from
the [@prettyprinter@](https://hackage.haskell.org/package/prettyprinter)
library.
You can render QBE IL source files, or any part of them, with something like:

> render :: Pretty a => a -> Text
> render = renderStrict . layoutPretty defaultLayoutOptions . pretty

>>> render $ Ret $ Just $ ValTemporary "a"
"ret %a"
>>> Text.putStrLn $ render $ Program [] [] [FuncDef [] Nothing "main" …
function w $main () {
@start
⋮
-}
module Language.QBE
(
-- * Identifiers
RawIdent
, Sigil(..)
, Ident(..)
-- * Types
, BaseTy(..)
, ExtTy(..)
-- * Constants
, Const(..)
-- * Linkage
, Linkage(..)
-- * Definitions
, Alignment
, Size
, Amount
-- ** Aggregate types
, TypeDef(..)
, SubTy(..)
-- ** Data
, DataDef(..)
, DataItem(..)
, Field(..)
-- ** Functions
, FuncDef(..)
, AbiTy(..)
, Param(..)
, Variadic(..)
, prettyVariadic
-- * Control
, Val(..)
, Block(..)
, Jump(..)
-- * Instructions
, Phi(..)
, PhiArg(..)
, Inst(..)
, Assignment(..)
, pattern (:=)
, IntRepr(..)
, BinaryOp(..)
, Comparison(..)
, Arg(..)
-- * Program
, Program(..)
) where

import Data.Text (Text)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import Data.ByteString (ByteString)
import Data.Word (Word64)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Maybe (maybeToList)
import Prettyprinter
  ( Pretty(pretty), Doc, (<+>), vsep, hsep, hang, punctuate, group, flatAlt
  , space, encloseSep, tupled, comma, equals, braces, lbrace, rbrace )
-- Instances
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)
import Data.String (IsString)

-- * Identifiers
----------------

-- | A raw identifier string, with no sigil information attached
type RawIdent = ShortText

-- | Sigils are used to differentiate the verious types of 'Ident'ifier.
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

-- | Base types
data BaseTy
  = Word -- ^ @w@
  | Long -- ^ @l@
  | Single -- ^ @s@
  | Double -- ^ @d@
  deriving (Show, Eq)

instance Pretty BaseTy where
  pretty Word   = pretty 'w'
  pretty Long   = pretty 'l'
  pretty Single = pretty 's'
  pretty Double = pretty 'd'

-- | Extended types
data ExtTy
  = BaseTy BaseTy
  | Byte -- ^ @b@
  | HalfWord -- ^ @h@
  deriving (Show, Eq)

instance Pretty ExtTy where
  pretty (BaseTy baseTy) = pretty baseTy
  pretty Byte = pretty 'b'
  pretty HalfWord = pretty 'h'

-- * Constants
--------------

-- | Constant/immediate
data Const
  -- MAYBE just use a signed type
  = CInt Bool Word64 -- ^ 64 bit integer. The 'Bool' is whether to negate.
  | CSingle Float -- ^ Single-precision float
  | CDouble Double -- ^ Double-precision float
  | CGlobal (Ident 'Global) -- ^ Global symbol
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
  = Export -- ^ Marks the defined item as visible outside the current file's scope
  | Section ShortText (Maybe Text) -- ^ Section name, with optional linker flags
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

-- | Aggregate type
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

-- | A type that can be part of an aggregate type
data SubTy
  = SubExtTy ExtTy
  | SubAggregateTy (Ident 'AggregateTy)
  deriving (Show, Eq)

instance Pretty SubTy where
  pretty (SubExtTy extTy) = pretty extTy
  pretty (SubAggregateTy ident) = pretty ident

-- ** Data
----------

-- | Global object definition
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

-- TODO use record syntax on long types like this one
-- | Function definition. The 'Maybe (Ident \'Temporary)' is the environment
data FuncDef = FuncDef [Linkage] (Maybe AbiTy) (Ident 'Global) (Maybe (Ident 'Temporary)) [Param] Variadic (NonEmpty Block)
  deriving (Show, Eq)

instance Pretty FuncDef where
  pretty (FuncDef linkage abiTy ident env params variadic blocks) = vsep
    [ vsep $ pretty <$> linkage
    , "function" <+> pretty abiTy <+> pretty ident <+> tupled (
        maybeToList (("env" <+>) . pretty <$> env)
        ++ fmap pretty params
        ++ maybeToList (prettyVariadic variadic)
      ) <+> lbrace
    , vsep $ toList $ pretty <$> blocks
    , rbrace
    ]

data AbiTy = AbiBaseTy BaseTy | AbiAggregateTy (Ident 'AggregateTy)
  deriving (Show, Eq)

instance Pretty AbiTy where
  pretty (AbiBaseTy baseTy) = pretty baseTy
  pretty (AbiAggregateTy ident) = pretty ident

-- | Function parameter
data Param = Param AbiTy (Ident 'Temporary)
  deriving (Show, Eq)

instance Pretty Param where
  pretty (Param abiTy ident) = pretty abiTy <+> pretty ident

-- | Indicates the presence or absence of a variadic marker
data Variadic = Variadic | NoVariadic
  deriving (Show, Eq)

-- | 'Variadic' → @Just "..."@
-- 'NoVariadic' → @Nothing@
prettyVariadic :: Variadic -> Maybe (Doc a)
prettyVariadic Variadic = Just "..."
prettyVariadic NoVariadic = Nothing

-- * Control
------------

-- | Value, either an immediate or a global or temporary identifier.
data Val
  = ValConst Const
  | ValTemporary (Ident 'Temporary)
  | ValGlobal (Ident 'Global)
  deriving (Show, Eq)

instance Pretty Val where
  pretty (ValConst c) = pretty c
  pretty (ValTemporary ident) = pretty ident
  pretty (ValGlobal ident) = pretty ident

-- | Block of instructions beginning with a label and ending with a jump
data Block = Block (Ident 'Label) [Phi] [Inst] Jump
  deriving (Show, Eq)

instance Pretty Block where
  pretty (Block ident phis insts jump) = hang 4 $ vsep $ concat
    [ [pretty ident]
    , pretty <$> phis
    , pretty <$> insts
    , [pretty jump]
    ]

-- | Jump instructions
data Jump
  = Jmp (Ident 'Label) -- ^ Unconditional jump
  | Jnz Val (Ident 'Label) (Ident 'Label) -- ^ Conditional jump
  | Ret (Maybe Val) -- ^ Function return
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

-- MAYBE change [PhiArg] to Map (Ident 'Label) Val
-- | Phi instruction
data Phi = Phi Assignment [PhiArg]
  deriving (Show, Eq)

instance Pretty Phi where
  pretty (Phi assignment args) =
    pretty assignment <+> "phi" <+> hsep (punctuate comma $ pretty <$> args)

-- | Phi instruction argument, associating a 'Val' to a 'Label'
data PhiArg = PhiArg (Ident 'Label) Val
  deriving (Show, Eq)

instance Pretty PhiArg where
  pretty (PhiArg label val) = pretty label <+> pretty val

-- | Instruction
data Inst
  -- Arithmetic and Bits
  = BinaryOp Assignment BinaryOp Val Val -- ^ Binary arithmetic and bit operations
  | Neg Assignment Val -- ^ @neg@
  -- Memory
  -- | @stored@/@stores@/@storel@/@storew@/@storeh@/@storeb@
  | Store ExtTy Val Val
  -- MAYBE collapse all the Loads in a single Load constructor and just discard
  -- the intrepr when unused.
  -- | @loadw@/@loadl@/@loads@/@loadd@
  | Load Assignment BaseTy Val
  -- | @loadsw@/@loaduw@
  | LoadW Assignment IntRepr Val
  -- | @loadsh@/@loaduh@
  | LoadH Assignment IntRepr Val
  -- | @loadsb@/@loadub@
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
  -- | @cast@
  | Cast Assignment Val
  -- | @copy@
  | Copy Assignment Val
  -- Calls
  -- | @call@. The fields are: assignment, function name, environment, arguments, variadic arguments
  | Call (Maybe (Ident 'Temporary, AbiTy)) Val (Maybe Val) [Arg] [Arg]
  -- Variadic
  -- | @vastart@, initializes a variable argument list
  | VaStart (Ident 'Temporary)
  -- | @vaarg@, fetches the next argument from a variable argument list
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

-- | Represents the @%x =t@ part of an instruction.
data Assignment = Assignment (Ident 'Temporary) BaseTy
  deriving (Show, Eq)

-- | Infix synonym of 'Assignment'
pattern (:=) :: Ident 'Temporary -> BaseTy -> Assignment
pattern (:=) ident ty = Assignment ident ty

instance Pretty Assignment where
  pretty (Assignment ident ty) = pretty ident <+> equals <> pretty ty

-- | Integer representation
data IntRepr = Signed | Unsigned
  deriving (Show, Eq)

-- | Binary arithmetic and bit operations
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

-- | Function argument
data Arg = Arg AbiTy Val
  deriving (Show, Eq)

instance Pretty Arg where
  pretty (Arg abiTy val) = pretty abiTy <+> pretty val

-- * Program
------------

-- | Datatypre representing a QBE IL source file
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

-- | Like 'list' and 'tupled', but with braces
braced :: [Doc ann] -> Doc ann
braced = group . encloseSep (flatAlt "{ " "{")
                            (flatAlt " }" "}")
                            ", "
