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
  ( NP (Nil, (:*)), showNP
  , LiftFunction, Multiplicity (Many, One)
  , CurryNP
  , UncurryNP'Fst, UncurryNP'Snd, UncurryNP
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
type family LiftFunction f (m :: Type -> Type) (p :: Multiplicity) where
  LiftFunction (a -> b -> c) m p = m a %p-> LiftFunction (b -> c) m p
  LiftFunction      (a -> b) m p = m a %p-> m b
  LiftFunction           (b) m _ = m b

-- | Convert a NP function to a curried function.
type family CurryNP a b (p :: Multiplicity) where
  CurryNP (NP (a:as)) b p = a %p-> CurryNP (NP as) b p
  CurryNP (NP    '[]) b _ = b

type family UncurryNP'Fst f :: [Type] where
  UncurryNP'Fst (a1 %p-> a2 %_-> g) = a1 : UncurryNP'Fst (a2 %p-> g)
  UncurryNP'Fst         (a1 %_-> b) = a1 : UncurryNP'Fst (b)
  UncurryNP'Fst                 (b) = '[]

type family UncurryNP'Snd f :: Type where
  UncurryNP'Snd (a1 %p-> a2 %_-> g) = UncurryNP'Snd (a2 %p-> g)
  UncurryNP'Snd       (a1   %_-> b) = UncurryNP'Snd (b)
  UncurryNP'Snd                 (b) = b

type family UncurryNP'Multiplicity f :: Multiplicity where
  UncurryNP'Multiplicity (a1 %_-> a2 %p-> g) = UncurryNP'Multiplicity (a2 %p-> g)
  UncurryNP'Multiplicity         (a1 %p-> b) = p
  UncurryNP'Multiplicity                 (b) = Many

type UncurryNP f = NP (UncurryNP'Fst f) %(UncurryNP'Multiplicity f)-> UncurryNP'Snd f
