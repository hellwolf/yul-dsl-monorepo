{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MonoLocalBinds     #-}

{-|

Copyright   : (c) Miao ZhiCheng, 2023
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : portable

= Description

The objects in the YulDSL symmetrical monoidal category consist of ABI primitive types, their products and their
heterogeneous lists.

-}

module LoliYul.Core.YulDSL.Obj
  ( YulObj (..)
  , YulO1, YulO2, YulO3, YulO4, YulO5
  , YulVal
  , YulNum
  ) where

import           Data.Constraint              (Dict (..))
import           Data.Typeable                (Typeable)
import           GHC.TypeNats                 (KnownNat)

import           Control.Category.Constrained (ProdObj (..), type (⊗))

import           LoliYul.Core.ContractABI


-- | YulDSL SMC objects.
class (Show a, Typeable a) => YulObj a where
  prod_objs :: forall b c. a ~ (b ⊗ c) => Dict (YulObj b, YulObj c)
  prod_objs = error "YulObj::prod_objs is only defined for a⊗b"

-- Convenient aliases for declaring YulObj constraints.

type YulO1 a = YulObj a
type YulO2 a b = (YulObj a, YulObj b)
type YulO3 a b c = (YulObj a, YulObj b, YulObj c)
type YulO4 a b c d = (YulObj a, YulObj b, YulObj c, YulObj d)
type YulO5 a b c d e = (YulObj a, YulObj b, YulObj c, YulObj d, YulObj e)

-- YulObj instances for ABI primitive types.

instance YulObj ()
instance YulObj BOOL
instance YulObj ADDR
instance (Typeable s, KnownNat n) => YulObj (INTx s n)
instance YulObj BYTES

-- | Yul value objects.
class (YulObj a, ABIValue a) => YulVal a

instance YulVal BOOL
instance YulVal ADDR
instance (Typeable s, KnownNat n) => YulVal (INTx s n)

-- | Yul number-value objects.
class (YulObj a, Num a) => YulNum a

instance (Typeable s, KnownNat n) => YulNum (INTx s n)

-- | YulObj Maybe typed instance.
instance YulObj a => YulObj (Maybe a)

-- | YulObj product instance.
instance (YulObj a, YulObj b) => YulObj (a ⊗ b) where
  prod_objs = Dict

-- | YulObj product instance for linear-smc library.
instance ProdObj YulObj where
  prodobj = Dict
  objprod = prod_objs
  objunit = Dict

-- | YulObj heterogeneous list instance.
instance (YulObj a, YulObj b) => YulObj (a :> b)
