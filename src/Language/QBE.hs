{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.QBE where

import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import Data.List.NonEmpty (NonEmpty)
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
newtype Ident (t :: Sigil) = Ident RawIdent
  deriving (Show, Eq, Ord, IsString, Binary, NFData, Hashable)

-- * Types
----------

data BaseTy = Word | Long | Single | Double
  deriving (Show, Eq)

data ExtTy = BaseTy BaseTy | Byte | HalfWord
  deriving (Show, Eq)

-- * Constants
--------------

data Const
  = CInt Bool Word64 -- ^ The 'Bool' is whether to negate
  | CSingle Float
  | CDouble Double
  | CGlobal (Ident 'Global)
  deriving (Show, Eq)

-- * Linkage
------------

data Linkage
  = Export
  | Section ShortText (Maybe Text)
  deriving (Show, Eq)

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

data SubTy
  = SubExtTy
  | SubAggregateTy (Ident 'AggregateTy)
  deriving (Show, Eq)

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

data Block = Block (Ident 'Label) [Phi] [Inst] Jump
  deriving (Show, Eq)

data Jump
  = Jmp (Ident 'Label)
  | Jnz Val (Ident 'Label) (Ident 'Label)
  | Ret (Maybe Val)
  deriving (Show, Eq)

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
