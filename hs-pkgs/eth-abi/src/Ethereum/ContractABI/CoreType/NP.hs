{-# LANGUAGE LinearTypes  #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : GHC2024

= Description

Ethereum contract ABI compatible tuples encoded as n-ary products (NP).

-}
module Ethereum.ContractABI.CoreType.NP
  ( NP (..), showNP
  , LiftFunction, Multiplicity (Many, One)
  , CurryNP, UncurriableNP (..), BuildableNP (..)
  , module Internal.Data.Type.List
  ) where

-- base
import           Data.Kind                        (Type)
import           Data.List                        (intercalate)
import           GHC.Base                         (Multiplicity (..))
-- constraints
import           Data.Constraint                  (Dict (Dict))
--
import           Ethereum.ContractABI.ABITypeable (ABITypeable (..))
import           Internal.Data.Type.List


-- | N-ary product simplified form comparing to the sop package.
data NP :: [Type] -> Type where
  Nil  :: NP '[]
  (:*) :: ABITypeable x => x -> NP xs -> NP (x : xs)
infixr 5 :*

-- | Existential wrapper of any 'NP' values.
data AnyNP = forall as. MkAnyNP (NP as)

-- | Show a NP as a list of strings.
showNP :: AnyNP -> [String]
showNP (MkAnyNP (Nil))     = []
showNP (MkAnyNP (a :* as)) = [show a] <> showNP (MkAnyNP as)

instance ABITypeable (NP '[]) where
  type instance ABITypeDerivedOf (NP '[]) = NP '[]
  abiTypeInfo = []

instance (ABITypeable x, ABITypeable (NP xs)) => ABITypeable (NP (x : xs)) where
  type instance ABITypeDerivedOf (NP (x : xs)) = NP (x : xs)
  abiTypeInfo = abiTypeInfo @x <> abiTypeInfo @(NP xs)

-- | ABI typeable unit.
instance ABITypeable () where
  type instance ABITypeDerivedOf () = NP '[]
  abiToCoreType () = Nil
  abiFromCoreType Nil = ()

-- | ABI typeable tuple.
instance (ABITypeable a1, ABITypeable a2) => ABITypeable (a1, a2) where
  type instance ABITypeDerivedOf (a1, a2) = NP '[a1, a2]
  abiProdObjs = Dict
  abiToCoreType (a1, a2) = a1 :* a2 :* Nil
  abiFromCoreType (a1 :* a2 :* Nil) = (a1, a2)

instance Show (NP '[]) where
  show _ = "()"

instance Show x => Show (NP (x : xs)) where
  show as = "(" ++ intercalate "," (showNP (MkAnyNP as)) ++ ")"

-- | Build a new currying function type from the function signature @f@ with a type function @m@ for each of its
-- arguments that is also linearity polymorphic to @p@.
type family LiftFunction (f :: Type) (m :: Type -> Type) (p :: Multiplicity) where
  LiftFunction (a -> b -> c) m p = m a %p-> LiftFunction (b -> c) m p
  LiftFunction (a -> b) m p = m a %p-> m b
  LiftFunction b m _ = m b

-- | Convert a NP function to a curried function.
type family CurryNP a b where
  CurryNP (NP '[]) b = b
  CurryNP (NP (a:as)) b = a -> CurryNP (NP as) b

-- | Uncurriable functions for 'NP' types with result type @b@.
class UncurriableNP m f xs b where
  -- | Function 'f' that takes in @m (NP xs)@ and outputs a @m b@.
  uncurryNP :: f -> m (NP xs) -> m b

class BuildableNP m x xs where
  buildNP :: m x -> m (NP xs) -> m (NP (x:xs))
