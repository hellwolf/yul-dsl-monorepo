{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LinearTypes #-}

module Control.Category.Constrained where

import Prelude hiding ((.),id)
import Data.Kind
import Data.Constraint
import Data.Type.Equality
import Data.Array(Ix)

type O2 k a b = (Obj k a, Obj k b)
type O3 k a b c =
  (Obj k a, Obj k b, Obj k c)
type O4 k a b c d =
  (Obj k a, Obj k b, Obj k c, Obj k d)

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)
  

class Trivial a
instance Trivial a

instance ProdObj Trivial where
  prodobj = Dict
  objprod = Dict
  objunit = Dict


class Category k where
  type Obj k :: Type -> Constraint {-<-}
  type Obj k = Trivial {->-}
  id   :: Obj k a => a `k` a
  (∘)  ::   (Obj k a, Obj k b, Obj k c) =>
             (b `k` c) -> (a `k` b) -> a `k` c

infixl 8 .
infixl 8 ∘

(.) :: (Category k, O3 k a b c) => k b c -> k a b -> k a c
(.) = (∘)

class ProdObj con where
  prodobj :: (con a, con b) => Dict (con (a⊗b))
  objprod :: forall z a b. (z ~ (a⊗b), con z) => Dict (con a, con b)
  objunit :: Dict (con ())

objProd :: forall k a b z. (z ~ (a⊗b), Obj k z, Monoidal k) => Dict (Obj k a, Obj k b)
objProd = objprod

prodObj ::  forall k a b. (Monoidal k, Obj k a, Obj k b) => Dict (Obj k (a⊗b))
prodObj = prodobj

unitObj ::  forall k. (Monoidal k) => Dict (Obj k ())
unitObj = objunit


infixr 0 //
(//) :: Dict c -> (c => k) -> k
Dict // k = k

type a ⊗ b = (a,b)
infixr 7 ⊗
type TensorClosed (con :: Type -> Constraint) =
  forall x y. (con x, con y) => con (x ⊗ y) :: Constraint


class ({-<-}ProdObj (Obj k),{->-}Category k) => Monoidal k where
  (×)      :: {-<-}(Obj k a, Obj k b, Obj k c, Obj k d) =>{->-} (a `k` b) -> (c `k` d) -> (a ⊗ c) `k` (b ⊗ d)
  swap     :: {-<-}(Obj k a, Obj k b) =>{->-} (a ⊗ b) `k` (b ⊗ a)
  assoc    :: {-<-}(Obj k a, Obj k b, Obj k c) =>{->-} ((a ⊗ b) ⊗ c) `k` (a ⊗ (b ⊗ c))
  assoc'   :: {-<-}(Obj k a, Obj k b, Obj k c) =>{->-} (a ⊗ (b ⊗ c)) `k` ((a ⊗ b) ⊗ c)
  unitor   :: {-<-}(Obj k a) =>{->-} a `k` (a ⊗ ())
  unitor'  :: {-<-}(Obj k a) =>{->-} (a ⊗ ()) `k` a

class Monoidal k => Cartesian k where
  exl   ::  {-<-} forall a b. O2 k a b                     => {->-}   (a ⊗ b) `k` a
  exr   ::  {-<-} forall a b. O2 k a b                     => {->-}   (a ⊗ b) `k` b
  dis   ::  {-<-} forall a.   Obj k a                      => {->-}   a `k` ()
  dup   ::  {-<-} (Obj k a, Obj k (a⊗a))                   => {->-}   a `k` (a ⊗ a)
  (▵)   ::  {-<-} forall a b c. (Obj k a,Obj k b, Obj k c) => {->-}   (a `k` b) -> (a `k` c) -> a `k` (b ⊗ c)

  {-<-}
  {-# MINIMAL exl,exr,dup | exl,exr,(▵) | dis,dup | dis,(▵) #-}
  dis = disDefault
  dup = id ▵ id
  exl = exlDefault
  exr = exrDefault
  (▵) = (▵!)
  {->-}

disDefault :: forall k a. (Cartesian k, Obj k a) =>  a `k` ()
disDefault = exr . unitor
     \\ prodObj @k @a @()
     \\ unitObj @k

exlDefault :: forall k a b. (Cartesian k, O2 k a b) =>  (a ⊗ b) `k` a
exlDefault = unitor' . (id × dis)
          \\ prodObj @k @a @b
          \\ prodObj @k @a @()
          \\ unitObj @k

exrDefault :: forall k a b. (Cartesian k, O2 k a b) =>  (a ⊗ b) `k` b
exrDefault = unitor' ∘ swap ∘ (dis × id)
          \\ prodObj @k @a @b
          \\ prodObj @k @b @()
          \\ prodObj @k @() @b
          \\ unitObj @k

(▵!) :: forall k a b c. (Cartesian k, O3 k a b c) =>   (a `k` b) -> (a `k` c) -> a `k` (b ⊗ c)
f ▵! g = (f × g) . dup
          \\ prodObj @k @a @a
          \\ prodObj @k @b @c

cartesianCross :: (Obj k (b1 ⊗ b2), Obj k b3, Obj k c, Obj k b1,
                     Obj k b2, Cartesian k) =>
                    k b1 b3 -> k b2 c -> k (b1 ⊗ b2) (b3 ⊗ c)
cartesianCross a b = (a . exl) ▵ (b . exr)
cartesianUnitor :: forall a k. (Obj k a, Obj k (), Cartesian k) => a `k` (a ⊗ ())
cartesianUnitor = id ▵ dis

cartesianUnitor' :: forall a k. (Obj k a, Obj k (), Cartesian k) => (a ⊗ ()) `k` a
cartesianUnitor' = exl

cartesianSwap :: forall a b k. (Obj k a, Obj k b, Cartesian k) => (a ⊗ b) `k` (b ⊗ a)
cartesianSwap = exr ▵ exl
     \\ prodObj @k @a @b

cartesianAssoc :: forall a b c k. (Obj k a, Obj k b, Obj k c, Cartesian k) => ((a ⊗ b) ⊗ c) `k` (a ⊗ (b ⊗ c))
cartesianAssoc = (exl . exl) ▵ ((exr . exl) ▵ exr)
     \\ prodObj @k @(a,b) @c
     \\ prodObj @k @a @b
     \\ prodObj @k @b @c

cartesianAssoc' :: forall a b c k. (Obj k a, Obj k b, Obj k c, Cartesian k) => (a ⊗ (b ⊗ c)) `k` ((a ⊗ b) ⊗ c)
cartesianAssoc' = (exl ▵ (exl . exr)) ▵ (exr . exr)
     \\ prodObj @k @a @(b,c)
     \\ prodObj @k @a @b
     \\ prodObj @k @b @c



class Monoidal k => CoCartesian k where
  inl   :: {-<-} O2 k a b                                 => {->-} a `k` (a ⊗ b)
  inr   :: {-<-} O2 k a b                                 => {->-} b `k` (a ⊗ b)
  new   :: {-<-} forall a. (Obj k a)                      => {->-} () `k` a
  jam   :: {-<-} Obj k a                                  => {->-} (a⊗a) `k` a
  (▿)   :: {-<-} forall a b c. (Obj k a,Obj k b, Obj k c) => {->-} (b `k` a) -> (c `k` a) -> (b ⊗ c) `k` a

  {-<-}
  jam = id ▿ id
  new = newDefault
  (▿) = (▿!)
  {->-}

jamDefault :: (Obj k a, CoCartesian k) => (a⊗a) `k` a
jamDefault = id ▿ id

newDefault :: forall k a. (Obj k a, CoCartesian k) => () `k` a
newDefault = unitor' . inr
        \\ prodObj @k @a @()
        \\ unitObj @k

(▿!) ::  forall k a b c. (O3 k a b c, CoCartesian k) => (b `k` a) -> (c `k` a) -> (b ⊗ c) `k` a
f ▿! g = jam . (f × g)
            \\ prodObj @k @a @a
            \\ prodObj @k @b @c

transp :: forall a b c d k con . (con ~ Obj k, Monoidal k, O4 k a b c d, (forall α β. (con α, con β) => con (α,β)))
       => ((a,b) ⊗ (c,d)) `k` ((a,c) ⊗ (b,d))
transp = assoc' . (id × (assoc . (swap × id) . assoc')) . assoc

-- -- Poor man's infix arrows.
-- -- http://haskell.1045720.n5.nabble.com/Type-operators-in-GHC-td5154978i20.html
-- type a - (c :: * -> * -> *) = c a
-- type c > b                  = c b

-- infix 2 -
-- infix 1 >


class Cartesian k => Closed k where
  -- expObj' :: forall a b. SObj k a -> SObj k b -> SObj k (a -> b)
  apply :: O2 k a b => ((a -> b) ⊗ a) `k`  b
  curry :: O3 k a b c => ((a ⊗ b) `k` c) -> (a `k` (b -> c))


class Invertible k where
  dual :: (a `k` b) -> b `k` a

type Hopf k = (Cartesian k, CoCartesian k)
  -- (laws unstated as usual...)
  -- jam . dup = id
  -- etc.

instance Category (FUN x) where
  id x = x
  f ∘ g = \x -> f (g x)

instance Monoidal (FUN m) where
  (f × g) (a,b) = (f a, g b)
  assoc ((x,y),z) = (x,(y,z)) 
  assoc' (x,(y,z)) = ((x,y),z)  
  swap (x,y) = (y,x)
  unitor = (,())
  unitor' (x,()) = x

instance Cartesian (->) where
  exl = fst
  exr = snd
  (f ▵ g) x = (f x, g x)
  dup x = (x,x)

instance Closed (->) where
  apply (f,x) = f x
  curry = Prelude.curry

type Comparator k = forall a b b'. k a b -> k a b' -> Maybe (b :~: b')

class Category k => HasCompare k where
  compareMorphs :: Comparator k

-- | Equality-witnessing order type
data Order a b where
  LT, GT :: Order a b
  EQ :: Order a a

newtype Atom s = Atom s deriving (Bounded, Eq, Ord, Enum,Ix)
newtype Dual a = Dual a


class (ProdObj con) => AutonomousObj con where
  objDual :: forall a. (con a) => Dict (con (Dual a))
  dualObj :: forall z a. (z ~ Dual a, con z) => Dict (con a)

type Unit = ()

class ({-<-}AutonomousObj (Obj cat), {->-}Monoidal cat) => Autonomous cat where
  turn   :: {-<-}Obj cat a => {->-} Unit `cat` (Dual a ⊗ a)
  turn'  :: {-<-}Obj cat a => {->-} (a ⊗ Dual a) `cat` Unit
