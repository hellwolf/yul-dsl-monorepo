{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE TypeFamilies        #-}
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
  , UncurryNP'Fst, UncurryNP'Snd, UncurryNP'Multiplicity, UncurryNP
  , CurryNP'Head, CurryNP'Tail
  , CurryingNP (curryingNP, uncurryingNP)
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


-- | N-ary product with simplified than its homonym in the sop package.
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

-- | Lift a new currying function type from the simple function signature @f@ with a type function @m@ for each of its
--   arguments with multiplicity arrows in @p@.
type family LiftFunction f (m :: Type -> Type) (p :: Multiplicity) where
  LiftFunction (a -> b -> c) m p = m a %p-> LiftFunction (b -> c) m p
  LiftFunction      (a -> b) m p = m a %p-> m b
  LiftFunction           (b) m _ = m b

-- | Convert a function in ts NP form @np -> b@ to a curried function with multiplicity arrows in @p@.
--
--   Note: To add multiplicity-polymorphic arrows or to decorate arguments with additional type function, use
--   'LiftFunction'.
type family CurryNP np b where
  CurryNP (NP (a:as)) b = a -> CurryNP (NP as) b
  CurryNP (NP    '[]) b = b

-- | Uncurry the arguments of a function to a list of types.
type family UncurryNP'Fst f :: [Type] where
  UncurryNP'Fst (a1 %_-> a2 %_-> g) = a1 : UncurryNP'Fst (a2 -> g)
  UncurryNP'Fst         (a1 %_-> g) = a1 : UncurryNP'Fst (g)
  UncurryNP'Fst                 (b) = '[]

-- | Uncurry the result of a function.
type family UncurryNP'Snd f  where
  UncurryNP'Snd (a1 %p-> a2 %_-> g) = UncurryNP'Snd (a2 %p-> g)
  UncurryNP'Snd       (a1   %_-> g) = UncurryNP'Snd (g)
  UncurryNP'Snd                 (b) = b

-- | Uncurry and extract the multiplicity of the last arrow.
type family UncurryNP'Multiplicity f :: Multiplicity where
  UncurryNP'Multiplicity (a1 %_-> a2 %p-> g) = UncurryNP'Multiplicity (a2 %p-> g)
  UncurryNP'Multiplicity         (a1 %p-> b) = p
  UncurryNP'Multiplicity                 (b) = Many

-- | Uncurry a function to its NP form whose multiplicity of the last arrow is preserved.
type UncurryNP f = NP (UncurryNP'Fst f) %(UncurryNP'Multiplicity f)-> UncurryNP'Snd f

-- | Uncurry the head of arguments of an currying function.
type family CurryNP'Head f where
  CurryNP'Head (a1 %_-> a2 %_-> g) = a1
  CurryNP'Head         (a1 %_-> g) = a1
  CurryNP'Head                 (b) = ()

-- | Uncurry the tail of an currying function.
type family CurryNP'Tail f where
  CurryNP'Tail (a1 %_-> a2 %p-> g) = a2 %p-> CurryNP'Tail g
  CurryNP'Tail (        a1 %p-> g) = CurryNP'Tail g
  CurryNP'Tail                 (b) = b

class CurryingNP f (xs :: [Type]) b (m1 :: Type -> Type) (m2 :: Type -> Type) (p :: Multiplicity) where
  uncurryingNP :: forall.
                  ( xs ~ UncurryNP'Fst f
                  , b  ~ UncurryNP'Snd f
                  ) =>
                  LiftFunction f m1 p
                  %p-> m2 (NP xs)
                  %p-> m2 b

  curryingNP :: forall.
                ( xs ~ UncurryNP'Fst f
                , b  ~ UncurryNP'Snd f
                ) => (m1 (NP xs) %p-> m1 b)
                %p-> m1 (CurryNP'Head f)
                %p-> (LiftFunction (CurryNP'Tail f) m1 p)
