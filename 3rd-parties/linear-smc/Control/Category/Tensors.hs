{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
module Control.Category.Tensors (R,
                                 constant, zeroTensor, delta, contract, deriv, juggleDown, juggleUp, plus, (⋆),
                                 tensorEval, tensorEval1, tensorEmbed, tensorEmbed1,
                                 affinity, derivUsingAffinity,

                                 TensorCategory(..), CoordinateCategory(..), AutonomousObj(..),
                                 Group(..), VectorSpace(..), Dual(..)) where

import Data.Kind
import Control.Category.Constrained
import Control.Category.StructuredObject
import Control.Category.Linear.Internal
import qualified Control.Category.FreeCartesian as FC
import Control.Category.FreeCartesian ((▴))
import Prelude hiding (id, (.), Num(..), uncurry)

class Group v where
  (+)     :: v -> v -> v
  zero    :: v
  negate  :: v -> v

class Group v => VectorSpace scalar v where
  (*^)  ::  scalar -> v -> v

type DualClosed (con :: Type -> Constraint) =
  forall x. (con x) => con (Dual x) :: Constraint


class Autonomous cat => TensorCategory v cat where
  metric          :: {-<-}(O2 cat v Unit) =>{->-} (v ⊗ v) `cat` Unit
  cometric        :: {-<-}(O2 cat v Unit) =>{->-} Unit `cat` (v ⊗ v)
  derivSemantics  :: {-<-}O3 cat a b v =>{->-} (a `cat` b) -> ((v ⊗ a) `cat` b)
  default derivSemantics  :: {-<-}(O3 cat a b v, Obj cat ~ con, TensorClosed con, GroupCat cat, v ~ Atom s, OO s a, OO s b, CoordinateCategory v cat, con ()) =>{->-} (a `cat` b) -> ((v ⊗ a) `cat` b)
  derivSemantics = derivUsingAffinity


juggleDown :: (con ~ Obj cat, TensorClosed con, DualClosed con, AutonomousObj con, TensorCategory v cat, con v, con ())
  => Dual v `cat` v
juggleDown = compactHelper2 cometric

juggleUp :: (con ~ Obj cat, TensorClosed con, DualClosed con, AutonomousObj con, TensorCategory v cat, con v, con ())
  => v `cat` Dual v
juggleUp = compactHelper1 metric

compactHelper1 :: (Autonomous k, Obj k ~ con, DualClosed con, TensorClosed con, con a, con b, con ()) => k (a ⊗ b) () -> k a (Dual b)
compactHelper1 f = unitor' ∘ (id × f) ∘ assoc . (swap × id) . assoc' . (id × turn) ∘ unitor

compactHelper2 :: (Autonomous k, Obj k ~ con, DualClosed con, TensorClosed con, con a, con b, con ()) => k () (a ⊗ b) -> k (Dual a) b
compactHelper2 f = unitor' ∘ swap ∘ ((turn' ∘ swap) × id) ∘ assoc' ∘ (id × f) ∘ unitor


shuf ::
  ( Monoidal k
  , con ~ Obj k
  , TensorClosed con
  , con a, con s, con b
  ) => (a ⊗ (s ⊗ b)) `k` (s ⊗ (a ⊗ b))
shuf = assoc ∘ (swap × id) ∘ assoc'



class TensorCategory v cat => CoordinateCategory v cat  where
  partialDerivative :: O3 cat a b v => (a `cat` b) -> ((v ⊗ a) `cat` b)
  christoffel     :: {-<-}(Obj cat v)=>{->-} (v ⊗ v) `cat` v


derivUsingAffinity :: forall a b v s k con.
  (AutonomousObj con, TensorClosed con, OO s a, OO s b, v ~ Atom s,
   con a, con b, con v, con (),
   GroupCat k, CoordinateCategory v k, Obj k ~ con )
  => k a b -> k (v ⊗ a) b
derivUsingAffinity t = partialDerivative t + negate (t ∘ affinity) + (affinity ∘ (id × t))


type AdditiveCat scalar (cat :: Type -> Type -> Type) = forall a b. VectorSpace scalar (a `cat` b)
type GroupCat (cat :: Type -> Type -> Type) = forall a b. Group (a `cat` b)

data S cat r where
  Compose  :: {-<-}Obj cat x => {->-} (x `cat` Unit) -> FC.Cat cat {-<-}(Obj cat){->-} r x -> S cat r
  Plus   :: (Bool -> S cat r) ⊸ S cat r
type R cat r = P cat r Unit ⊸ S cat r



constant      :: (Monoidal cat, AdditiveCat scalar cat {-<-},con r, con a, Autonomous cat, Obj cat ~ con, DualClosed con, TensorClosed con, con (){->-})
  => scalar -> R cat r
constant s (Y u) = Compose (s *^ id) u




zeroTensor = tensorEmbed1 zero

plus f u = Plus (\b -> f b u) 

Compose t1 q1  ⋆- Compose t2 q2  = Compose (unitor' ∘ (t1 × t2)) (q1 ▴ q2)
Plus f         ⋆- t              = Plus (\c -> f c ⋆- t)
t              ⋆- Plus f         = Plus (\c -> t ⋆- f c)

deriv'         :: {-<-} forall v cat con r. (con v, TensorClosed con, con ~ Obj cat, con r, con (), Monoidal cat, ProdObj con) =>{->-} (TensorCategory v cat) => P cat r v ⊸ S cat r ⊸ S cat r
deriv'  (Y i)  (Compose t q)  = Compose (derivSemantics t) (i ▴ q)
deriv'  p      (Plus f )      = Plus (\c -> deriv' p (f c) )


deriv i r u = deriv' i (r u)

cartesianToMonoidal  :: {-<-}(Obj cat~con,Monoidal cat, ProdObj con, TensorClosed con, con (), con a, con b) => {->-} FC.Cat cat {-<-}con{->-} a b -> a `cat` b
cartesianToMonoidal = FC.toSMC

tensorEval0'   :: ({-<-}Obj cat ~ con, con a, TensorClosed con, ProdObj con, con (), {->-}Monoidal cat, GroupCat  cat) => S cat a -> a `cat` Unit
tensorEval0' u = case u of
  (Compose t q)  -> t ∘ cartesianToMonoidal q
  Plus f         -> tensorEval0' (f True) + tensorEval0' (f False)

tensorEval1 f          = tensorEval0' (uncurry f (Y (FC.embed unitor)))
tensorEval f         = unitor' . swap . (tensorEval1 (uncurry f) × id) . assoc' . (id × turn) . unitor
tensorEmbed1 f (Y q) (Y u) = Compose (f ∘ unitor') (q ▴ u)
tensorEmbed t i j     = tensorEmbed1 (turn' ∘ (t × id)) (merge (i,j))


delta'         :: {-<-}(Autonomous cat, TensorClosed obj, DualClosed obj, Obj cat ~ obj, obj a, obj (), obj r) => {->-}P cat r a ⊸ P cat r (Dual a) ⊸ S cat r
delta' (Y i) (Y j) = Compose turn' (i ▴ j)

delta i j u = eatU u (delta' i j)

type U cat r = P cat r Unit

eatU :: (con ~ Obj cat, TensorClosed con, ProdObj con, con r, con (), Monoidal cat) => U cat r ⊸ S cat r ⊸ S cat r
eatU (Y f) (Compose φ g) = Compose (φ ∘ unitor') (g ▴ f)
eatU p (Plus f) = Plus (\b -> eatU p (f b))


tensorEmbed   :: ({-<-}Obj cat ~ con, con a, con b, TensorClosed con, DualClosed con, ProdObj con, con (), {->-}Autonomous cat) => (a `cat` b) -> (forall r. {-<-} con r => {->-} P cat r a ⊸ P cat r (Dual b) ⊸ R cat r)
tensorEmbed1  :: ({-<-}Obj cat ~ con, con a, TensorClosed con, ProdObj con, con (), {->-}Monoidal cat) => (a `cat` Unit) -> (forall r. {-<-}  con r => {->-} P cat r a ⊸ R cat r)

tensorEval    :: ({-<-}Obj cat ~ con, con a, con b, DualClosed con, TensorClosed con, ProdObj con, con (), {->-}Autonomous cat, GroupCat  cat) => (forall r. {-<-} con r => {->-} P cat r a ⊸ P cat r (Dual b) ⊸ R cat r) -> a `cat` b
tensorEval1    :: ({-<-}Obj cat ~ con, con a, TensorClosed con, ProdObj con, con (), {->-}Monoidal cat, GroupCat cat) => (forall r. {-<-} con r => {->-} P cat r a ⊸ R cat r) -> a `cat` Unit

zeroTensor    :: (GroupCat cat, Autonomous cat{-<-}, Obj cat ~ con, con r, con a,  DualClosed con, TensorClosed con, con (){->-}) => P cat r a ⊸ R cat r

plus          :: (Bool -> R cat r) ⊸ R cat r
(⋆)           :: (Monoidal cat {-<-}, O2 cat r (), TensorClosed con, con ~ Obj cat {->-}) => R cat r ⊸ R cat r ⊸ R cat r
(⋆-)      :: (Monoidal cat, {-<-} O2 cat r (), TensorClosed con, con ~ Obj cat {->-}) => S cat r ⊸ S cat r ⊸ S cat r
(f ⋆ g) u = dupU u & \(u1,u2) -> f u1 ⋆- g u2

dupU :: (Monoidal cat,con ~ Obj cat, TensorClosed con, ProdObj con, con r, con Unit) => U cat r ⊸ (U cat r,U cat r)
dupU = split ∘ (encode unitor)

delta         :: {-<-}(Autonomous cat, TensorClosed obj, DualClosed obj, Obj cat ~ obj, obj a, obj (), obj r) => {->-}P cat r a ⊸ P cat r (Dual a) ⊸ R cat r
contract :: {-<-}(Autonomous cat, con ~ Obj cat, con a, con r, con (), con (Dual a), TensorClosed con, DualClosed con) => {->-}(P cat r (Dual a) ⊸ P cat r a ⊸ R cat r) ⊸ R cat r
contract f u = uncurry (uncurry f) (encode ((turn × id) . unitor) u) 

deriv         :: {-<-}forall v cat con r.  (con v, TensorClosed con, con ~ Obj cat, con r, con (), Monoidal cat, ProdObj con) =>{->-} (TensorCategory v cat) => P cat r v ⊸ R cat r ⊸ R cat r

uncurry   :: (Monoidal cat {-<-} , O3 cat r a b, con (), TensorClosed con, con ~ Obj cat{->-})  =>  (P cat r a ⊸ P cat r b ⊸ k) ⊸ (P cat r (a⊗b) ⊸ k)
uncurry f p = split p & \case (a,b) -> f a b

(&) ::  a ⊸ (a ⊸ b) ⊸ b
x & f = f x


affinity :: forall v a s k con.
            (con ~ Obj k, OO s a, con a, con v, con (), v ~ Atom s,
             TensorClosed con, AutonomousObj con, GroupCat k,
             TensorCategory (Atom s) k,CoordinateCategory (Atom s) k)
         => k (Atom s ⊗ a) a
affinity = aff getRepr

aff :: forall s k con a.
       (con ~ Obj k, con a, con (Atom s), con (),
        TensorClosed con, AutonomousObj con, TensorCategory (Atom s) k,
        GroupCat k, CoordinateCategory (Atom s) k)
    => Repr s a -> k (Atom s ⊗ a) a
aff = \case
  RAtom -> christoffel
  RUnit -> zero
  RPair p q ->
    objprod @con @a //
    ((aff p × id) ∘ assoc') + ((id × aff q) ∘ shuf)
  RDual q ->
    dualObj @con @a //
    negate (unitor' . swap .
           (turn' × id) .
           assoc' . (id × swap) . assoc .
           ((( aff q × id) . assoc' . (id × (swap . turn) ) . unitor) × id))
