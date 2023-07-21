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
'YulCat'. Further more, the 'YulCat' is instantiated as a "Symmetric Monoidal Category" (SMC). Being a SMC enables
the possibility for compiling linearly-typed functions in Haskell directly to the 'YulCat', that is believed to
greatly enhance the ergonomics of programming in 'YulCat'.

YulCat is designed to be a category. The objects in this category are instances of @ABIType@.

The classification objects in the YulCat symmetrical monoidal category:

  * 'YulObj'
  * 'YulVal'
  * 'YulNum'

-}

module YulDSL.Core.YulCat
  ( YulObj
  , YulO1, YulO2, YulO3, YulO4, YulO5
  , YulVal
  , YulNum
  , YulCat (..)
  , emptyYulCat
  , Fn (..), AnyFn (..), showFnSpec
  , YulCode (..)
  ) where

-- base
import           Data.Typeable           (Proxy (..), Typeable, typeRep)
import           GHC.TypeNats            (KnownNat)
--
import           YulDSL.Core.Coerce
import           YulDSL.Core.ContractABI

-- | All objects in the category is simply a 'ABIType'.
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
  -- | Apply the function object over an argument.
  YulApFun :: forall a b. YulO2 a b => String -> YulCat a b
  -- | Mapping over a list.
  YulMap   :: forall a b. YulO2 a b => YulCat a b -> YulCat [a] [b]
  -- | Folding over a list from the left.
  YulFoldl :: forall a b. YulO2 a b => YulCat (b, a) b -> YulCat [a] b
  -- | EVM Call.
  YulCall  :: forall a b. YulO2 a b => YulCat ((CallSpec a b), a) (Maybe b)
  -- | If-then-else.
  YulITE   :: forall a  . YulO1 a   => YulCat (BOOL, (a, a)) a

  -- YulVal Primitives
  --
  -- - Boolean Operatiosn
  YulNot :: YulCat BOOL BOOL
  YulAnd :: YulCat (BOOL, BOOL) BOOL
  YulOr  :: YulCat (BOOL, BOOL) BOOL
  -- - Num Types
  YulNumNeg :: forall a. YulNum a => YulCat a a
  YulNumAdd :: forall a. YulNum a => YulCat (a, a) a
  -- | Number comparison with a three-way boolean-switches (LT, EQ, GT).
  YulNumCmp :: forall a b. (YulNum a, YulObj b) => (b, b, b) -> YulCat (a, a) b
  -- - Contract ABI Serialization
  YulAbiEnc :: YulObj a => YulCat a BYTES
  YulAbiDec :: YulObj a => YulCat BYTES (Maybe a)

  -- Storage Primitives
  --
  YulSGet :: forall a. YulVal a => YulCat ADDR a
  YulSPut :: forall a. YulVal a => YulCat (ADDR, a) ()

-- | Internal function object.
--
--   Note:
--   - TypeReps are attached for showing AnyFn.
data Fn a b = Defun String (YulCat a b)
data AnyFn = forall a b. YulO2 a b => MkAnyFn (Fn a b)

-- | Print function specification.
showFnSpec :: forall a b. YulO2 a b => Fn a b -> String
showFnSpec (Defun name _) = "function " <> name <> "("
  <> show (typeRep (Proxy :: Proxy a)) <> ") -> "
  <> show (typeRep (Proxy :: Proxy b))

-- | YulCat are embedded in the yul code blocks.
data YulCode = MkYulCode { yulFunctions :: [AnyFn]
                         , yulInitCode  :: YulCat () ()
                         }

-- | Handy empty YulCat.
emptyYulCat :: YulCat () ()
emptyYulCat = YulId

deriving instance Show (YulCat a b)

instance YulO2 a b => Show (Fn a b) where
  show fn@(Defun _ cat) = showFnSpec fn <> ":\n"
    <> show cat

instance Show AnyFn where
  show (MkAnyFn fn) = show fn

instance Show YulCode where
  show (MkYulCode fns initCode) = foldr (flip (<>) . (<> "\n") . show) "" fns
    <> "Init code:\n" <> show initCode
