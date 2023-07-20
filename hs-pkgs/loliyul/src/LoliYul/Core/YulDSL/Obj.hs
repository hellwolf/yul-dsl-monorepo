{-# OPTIONS_GHC -Wno-orphans #-}

{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

The classification objects in the YulDSL symmetrical monoidal category:

  * 'YulObj'
  * 'YulVal'
  * 'YulNum'

-}

module LoliYul.Core.YulDSL.Obj
  ( YulObj
  , YulO1, YulO2, YulO3, YulO4, YulO5
  , YulVal
  , YulNum
  ) where

import           Data.Constraint              (Dict (..))
import           Data.Typeable                (Typeable)
import           GHC.TypeNats                 (KnownNat)

import           Control.Category.Constrained (ProdObj (..))

import           LoliYul.Core.ContractABI

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

-- | Instance for linear-smc 'ProdObj' for the objects in the category.
instance ProdObj YulObj where
  prodobj = Dict
  objprod = maybe_prod_objs
  objunit = Dict
