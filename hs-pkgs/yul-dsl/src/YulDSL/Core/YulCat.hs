{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental


= Description

[Yul](https://docs.soliditylang.org/en/latest/yul.html) is an intermediate language that is part of the [solidity
compiler](https://docs.soliditylang.org/en/latest/). It is by-design aspiring to be compiled to bytecode for
different backends, while at the moment it is for [Ethereum Virtual
Machine](https://ethereum.org/en/developers/docs/evm/) (EVM).

This module provides an "Embedded (in Haskell) Domain Specific Language" (eDSL) for programming in Yul, called
'YulCat'.


Further more, the 'YulCat' is instantiated as a "Symmetric Monoidal Category" (SMC). Being a SMC enables the possibility
for compiling linearly-typed functions in Haskell directly to the 'YulCat', that is believed to greatly enhance the
ergonomics of programming in 'YulCat'.

YulCat is designed to be a category. The objects in this category are instances of @ABIType@.

The classification objects in the YulCat symmetrical monoidal category:

  * 'YulObj'
  * 'YulVal'
  * 'YulNum'

-}

module YulDSL.Core.YulCat
  ( YulObj, YulO1, YulO2, YulO3, YulO4, YulO5
  , YulVal, YulNum
  , YulCat (..), AnyYulCat (..)
  , Fn (..), FnName, mkFn', AnyFn (..), showFnSpec
  ) where

-- base
import           Data.Char               (ord)
import           Data.Typeable           (Typeable)
import           GHC.Integer             (xorInteger)
import           GHC.TypeNats            (KnownNat)
import           Text.Printf             (printf)
-- byteString
import qualified Data.ByteString.Char8   as B
--
import           YulDSL.Core.Coerce
import           YulDSL.Core.ContractABI

-- | All objects in the 'YulCat' category is simply a 'ABIType'.
type YulObj  = ABIType

-- Convenient aliases for declaring YulObj constraints.

type YulO1 a = YulObj a
type YulO2 a b = (YulObj a, YulObj b)
type YulO3 a b c = (YulObj a, YulObj b, YulObj c)
type YulO4 a b c d = (YulObj a, YulObj b, YulObj c, YulObj d)
type YulO5 a b c d e = (YulObj a, YulObj b, YulObj c, YulObj d, YulObj e)

-- | Value-type objects in the category.
class (YulObj a, ABIValue a) => YulVal a

instance YulVal BOOL
instance YulVal ADDR
instance (Typeable s, KnownNat n) => YulVal (INTx s n)

-- | Number-type objects in the category.
class (YulVal a, Num a) => YulNum a

instance (Typeable s, KnownNat n) => YulNum (INTx s n)

-- | A category of GADT-style DSL for Yul that constructs morphisms between its objects (YulObj).
data YulCat a b where
  -- SMC Primitives
  --
  YulCoerce :: forall a b. (YulO2 a b, YulCoercible a b) => YulCat a b
  --
  YulId   :: forall a.       YulO2 a a     => YulCat a a
  YulComp :: forall a b c.   YulO3 a b c   => YulCat c b -> YulCat a c -> YulCat a b
  YulProd :: forall a b c d. YulO4 a b c d => YulCat a b -> YulCat c d -> YulCat (a, c) (b, d)
  YulSwap :: forall a b.     YulO2 a b     => YulCat (a, b) (b, a)
  YulDis  :: forall a.       YulO1 a       => YulCat a ()
  YulDup  :: forall a.       YulO1 a       => YulCat a (a, a)

  -- Control Flow Primitives
  --
  -- | Embed a constant value.
  YulEmbed :: forall a  . YulO1 a   => a -> YulCat () a
  -- | Apply the yul function over an object @a@.
  YulApFun :: forall a b. YulO2 a b => Fn a b -> YulCat a b
  -- | If-then-else.
  YulITE   :: forall a  . YulO1 a   => YulCat (BOOL, (a, a)) a
  -- | Mapping over a list.
  YulMap   :: forall a b. YulO2 a b => YulCat a b -> YulCat [a] [b]
  -- | Folding over a list from the left.
  YulFoldl :: forall a b. YulO2 a b => YulCat (b, a) b -> YulCat [a] b
  -- | EVM Call.
  YulCall  :: forall a b. YulO2 a b => YulCat ((CallSpec a b), a) (Maybe b)

  -- YulVal Primitives
  --
  -- * Boolean Operations
  YulNot :: YulCat BOOL BOOL
  YulAnd :: YulCat (BOOL, BOOL) BOOL
  YulOr  :: YulCat (BOOL, BOOL) BOOL
  -- * Num Types
  YulNumAdd :: forall a. YulNum a => YulCat (a, a) a
  YulNumNeg :: forall a. YulNum a => YulCat a a
  -- * Number comparison with a three-way boolean-switches (LT, EQ, GT).
  YulNumCmp :: forall a. YulNum a => (BOOL, BOOL, BOOL) -> YulCat (a, a) BOOL
  -- * Contract ABI Serialization
  YulAbiEnc :: YulObj a => YulCat a BYTES
  YulAbiDec :: YulObj a => YulCat BYTES (Maybe a)

  -- Storage Primitives
  --
  YulSGet :: forall a. YulVal a => YulCat ADDR a
  YulSPut :: forall a. YulVal a => YulCat (ADDR, a) ()

-- | Yul function name.
type FnName = String

-- | Yul function object.
data Fn a b where
  MkFn :: forall a b. YulO2 a b => FnName -> YulCat a b -> Fn a b

-- | Make a function with generated function name
mkFn' :: forall a b. YulO2 a b => YulCat a b -> Fn a b
mkFn' c = MkFn ("_" <> digest_yul_cat c) c

-- | Existential wrapper of the `YulCat`.
data AnyYulCat = forall a b. YulO2 a b => MkAnyYulCat (YulCat a b)

-- | Existential wrapper of the `Fn`.
data AnyFn = forall a b. YulO2 a b => MkAnyFn (Fn a b)

------------------------------------------------------------------------------------------------------------------------
-- Show Instances & Utilities
------------------------------------------------------------------------------------------------------------------------

-- | Print function specification.
showFnSpec :: forall a b. YulO2 a b => Fn a b -> String
showFnSpec (MkFn name _) = "function " <> name
  <> "(" <> abi_type_name @a <> ") -> " <> abi_type_name @b

-- | Bespoke show instance for YulCat.
--
--   Note:
--   * It is deliberately done so for compactness of the string representation of the 'YulCat'.
--   * It is meant also for strong equality checking of 'YulCat' used in yul object building.
instance Show (YulCat a b) where
  show YulCoerce             = "(coerce" <> abi_type_name' @a <> abi_type_name' @b <> ")"
  show YulId                 = "(id" <> abi_type_name' @a <> abi_type_name' @b <> ")"
  show (YulComp cb ac)       = show cb <> "∘" <> show ac
  show (YulProd ab cd)       = "(⊗(" <> show ab <> ")(" <> show cd <> "))"
  show YulSwap               = "(swap" <> abi_type_name' @a <> abi_type_name' @b <> ")"
  show YulDis                = "(dis" <> abi_type_name' @a <> ")"
  show YulDup                = "(dup" <> abi_type_name' @a <> ")"
  show (YulEmbed x)          = "{" <> show x <> "}" -- TODO: x should be escaped ideally, especially for equality checks
  show (YulApFun (MkFn n _)) = "(apfun" <> n <> ")"
  show YulITE                = "(ite" <> abi_type_name' @a <> ")"
  show YulNot                = "not"
  show YulAnd                = "and"
  show YulOr                 = "or"
  show YulNumAdd             = "(add" <> abi_type_name' @a <> ")"
  show YulNumNeg             = "(neg" <> abi_type_name' @a <> ")"
  show (YulNumCmp (i,j,k))   = "(cmp(" <> show i <> ")(" <> show j <> ")(" <> show k <> "))"
  show YulSGet               = "(sget" <> abi_type_name' @a <> ")"
  show YulSPut               = "(sput" <> abi_type_name' @a <> ")"
  show _                     = error "todo"

-- It's not as scary as it sounds. It produces a default function name for the cat.
digest_yul_cat :: YulCat a b -> String
digest_yul_cat = printf "%x" . digest_c8 . B.pack . show
  where c8 _ []     = 0
        c8 n [a]    = (2 ^ n) * toInteger(ord a)
        c8 n (a:as) = (2 ^ n) * toInteger(ord a) + c8 (n + 8) as
        digest_c8 bs = go_digest_c8 (B.splitAt 8 bs)
        go_digest_c8 (b, bs') = c8 (0 :: Integer) (B.unpack b) `xorInteger`
                                if B.length bs' == 0 then 0 else digest_c8 bs'

instance Eq AnyFn where
  (MkAnyFn (MkFn a a')) == (MkAnyFn (MkFn b b')) = a == b && digest_yul_cat a' == digest_yul_cat b'

instance Show (Fn a b) where
  show fn@(MkFn _ cat) = showFnSpec fn <> ": " <> show cat

instance Show AnyFn where
  show (MkAnyFn fn) = show fn
