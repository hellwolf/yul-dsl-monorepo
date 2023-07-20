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

This module provides an "Embedded (in Haskell) Domain Specific Language" (EDSL) for programming in Yul, called
'YulDSL'. Further more, the 'YulDSL' is instantiated as a "Symmetric Monoidal Category" (SMC). Being a SMC enables
the possibility for compiling linearly-typed functions in Haskell directly to the 'YulDSL', that is believed to
greatly enhance the ergonomics of programming in 'YulDSL'.

YulDSL is designed to be a category. The objects in this category are instances of @ABIType@.

The classification objects in the YulDSL symmetrical monoidal category:

  * 'YulObj'
  * 'YulVal'
  * 'YulNum'

-}

module YulDSL.Core.YulDSL
  ( YulObj
  , YulO1, YulO2, YulO3, YulO4, YulO5
  , YulVal
  , YulNum
  , YulDSL (..)
  , Fn (..)
  , module YulDSL.Core.Coerce
  ) where

-- base
import           Data.Typeable           (Typeable)
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

-- | A GADT-style DSL for Yul that constructs morphisms between its objects.
data YulDSL a b where
  -- SMC Primitives
  --
  YulCoerce :: forall a b. (YulO2 a b, YulCoercible a b) => YulDSL a b
  --
  YulId     :: forall a.       YulO2 a a     => YulDSL a a
  YulComp   :: forall a b c.   YulO3 a b c   => YulDSL c b -> YulDSL a c -> YulDSL a b
  YulProd   :: forall a b c d. YulO4 a b c d => YulDSL a b -> YulDSL c d -> YulDSL (a, c) (b, d)
  YulSwap   :: forall a b.     YulO2 a b     => YulDSL (a, b) (b, a)
  YulDis    :: forall a.       YulO1 a       => YulDSL a ()
  YulDup    :: forall a.       YulO1 a       => YulDSL a (a, a)

  -- Control Flow Primitives
  --
  -- | Embed a constant value.
  YulEmbed :: forall a  . YulO1 a   => a -> YulDSL () a
  -- | Apply the function object over an argument.
  YulApFun :: forall a b. YulO2 a b => String -> YulDSL a b
  -- | Mapping over a list.
  YulMap   :: forall a b. YulO2 a b => YulDSL a b -> YulDSL [a] [b]
  -- | Folding over a list from the left.
  YulFoldl :: forall a b. YulO2 a b => YulDSL (b, a) b -> YulDSL [a] b
  -- | EVM Call.
  YulCall  :: forall a b. YulO2 a b => YulDSL ((CallSpec a b), a) (Maybe b)
  -- | If-then-else.
  YulITE   :: forall a  . YulO1 a   => YulDSL (BOOL, (a, a)) a

  -- YulVal Primitives
  --
  -- - Boolean Operatiosn
  YulNot :: YulDSL BOOL BOOL
  YulAnd :: YulDSL (BOOL, BOOL) BOOL
  YulOr  :: YulDSL (BOOL, BOOL) BOOL
  -- - Num Types
  YulNumNeg :: forall a. YulNum a => YulDSL a a
  YulNumAdd :: forall a. YulNum a => YulDSL (a, a) a
  -- | Number comparison with a three-way boolean-switches (LT, EQ, GT).
  YulNumCmp :: forall a b. (YulNum a, YulObj b) => (b, b, b) -> YulDSL (a, a) b
  -- - Contract ABI Serialization
  YulAbiEnc :: YulObj a => YulDSL a BYTES
  YulAbiDec :: YulObj a => YulDSL BYTES (Maybe a)

  -- Storage Primitives
  --
  YulSGet :: forall a. YulVal a => YulDSL ADDR a
  YulSPut :: forall a. YulVal a => YulDSL (ADDR, a) ()

-- | Internal function object.
data Fn a b = Defun String (YulDSL a b) deriving Show

deriving instance Show (YulDSL a b)
