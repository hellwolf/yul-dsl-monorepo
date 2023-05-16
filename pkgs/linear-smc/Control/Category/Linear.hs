{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}


module Control.Category.Linear (
  -- Interface
  type P, unit, split, merge,
  encode, decode, (!:),
  -- Helpers for cartesian categories
  ignore, copy, discard
) where


import Data.Kind (Type)

import Prelude hiding ((.),id,curry,LT,GT,EQ)
import Control.Category.Constrained
import Control.Category.FreeCartesian as Cartesian
import Unsafe.Coerce
import qualified Control.Category.FreeSMC as SMC



-- pattern (:::) :: forall con (k :: Type -> Type -> Type) r a b.
--                    (Obj k r, Obj k a, Obj k b, Monoidal k, con (), (forall α β. (con α, con β) => con (α,β)), con ~ Obj k) =>
--                    P k r a ⊸ P k r b ⊸ P k r (a, b)
-- pattern x ::: y <- (split @con -> (x,y))
--   where x ::: y = merge @con (x,y)
--
-- infixr ::: -- GHC does not always see this change. rm -r dist/dante. T_T (ghc 8.8.4)


type P :: (Type -> Type -> Type) -> Type -> Type -> Type

unit     :: {-<-}forall k con r. (Obj k r, Monoidal k, con (), (forall α β. (con α, con β) => con (α,β)), con ~ Obj k)         => {->-}P k r ()
split    :: {-<-}forall con a b r k. (O3 k r a b, Monoidal k, con (), (forall α β. (con α, con β) => con (α,β)), con ~ Obj k) => {->-}P k r (a ⊗ b) ⊸ (P k r a, P k r b)
merge    :: {-<-}forall  con a b r k. (O3 k r a b, Monoidal k, con(), (forall α β. (con α, con β) => con (α,β)), con ~ Obj k) => {->-}(P k r a , P k r b) ⊸ P k r (a ⊗ b)
encode   :: {-<-} O3 k r a b => {->-}  (a `k` b) -> (P k r a ⊸ P k r b)
decode   :: {-<-} forall a b k con. (con (), con ~ Obj k, Monoidal k, con a, con b, (forall α β. (con α, con β) => con (α,β))) => {->-} (forall r. {-<-}Obj k r =>{->-} P k r a ⊸ P k r b) -> (a `k` b)

(!:) :: forall  con a b r k. (O3 k r a b, Monoidal k, con(), (forall α β. (con α, con β) => con (α,β)), con ~ Obj k)
       => P k r a ⊸ P k r b ⊸ P k r (a,b)
x !: y = merge (x,y)


data P k r a     where
  Y :: FreeCartesian k {-<-} (Obj k) {->-} r a -> P k r a
fromP :: P k r a -> FreeCartesian k {-<-} (Obj k) {->-} r a
fromP (Y f) = f

  
encode φ (Y f)    = Y (Embed φ ∘ f) -- put φ after f.
unit              = Y dis
split (Y f)       = (Y (exl ∘ f), Y (exr ∘ f)) 
merge (Y f, Y g)  = Y (f ▵ g)


decode f          = SMC.evalM (reduce (extract f))
extract           :: {-<-} (Obj k a, Obj k b) => {->-} (forall r. {-<-} Obj k r => {->-} P k r a ⊸ P k r b) -> FreeCartesian k {-<-} (Obj k) {->-} a b
extract f         = fromP (f (Y id))


---------------------------------------------------------------------
-- If the underlying category is cartesian, we have additionally:


ignore      :: (Monoidal k, {-<-} O3 k r a (), (forall α β. (con α, con β) => con (α,β)), con ~ Obj k {->-}) => P k r () ⊸ P k r a ⊸ P k r a
ignore f g  = encode unitor' (merge (g,f))
  
copy  :: (Cartesian k {-<-} , O2 k r a, (forall α β. (con α, con β) => con (α,β)), con ~ Obj k {->-} ) => P k r a ⊸ P k r (a ⊗ a)
copy  = encode dup
discard  :: (Cartesian k {-<-} , O2 k r a, (forall α β. (con α, con β) => con (α,β)), con ~ Obj k, con () {->-} ) => P k r a ⊸ P k r ()
discard  = encode dis






type FreeSMC = SMC.Cat

-- haskell-src-exts does not like the ' before constructors. It does not honour extensions either.
type Null = '[]
type Cons x xs = x ': xs

type family Prod (xs :: [Type])  where
  Prod Null = ()
  Prod (Cons x ys) = x ⊗ Prod ys


data Merge k {-<-}con{->-} a xs where
  (:+)   :: {-<-}(con x, con (Prod xs)) => {->-}FreeCartesian k {-<-}con{->-} a x -> Merge k {-<-}con{->-} a xs
         -> Merge k {-<-}con{->-}  a (Cons x xs)
  Nil    :: Merge k {-<-}con{->-} a Null

infixr :+


-- | expose does two things:
-- 1. push abstract morphisms (E, X) into the already processed part
-- 2. turn f ▵ g into a Merge

expose  ::  {-<-}(ProdObj con, forall α β. (con α, con β) => con (α,β), con (), con a, con b) => {->-}Cat cat {-<-}con{->-} a b ->
            (  forall x. {-<-}con (Prod x) =>{->-} FreeSMC cat {-<-}con{->-} (Prod x) b ->
               Merge cat {-<-}con{->-} a x -> k) -> k
expose (f1 :▵: f2) k      =  expose f1 $ \g1 fs1 ->
                             expose f2 $ \g2 fs2 ->
                             appendSorted fs1 fs2 $ \g fs ->
                             k ((g1 × g2) ∘ g) fs
expose (Embed ϕ :<: f) k  =  expose f $ \g fs ->
                             k (SMC.Embed ϕ ∘ g) fs
expose (E :<: _) k        = k id Nil
expose x k                = k unitor' (x :+ Nil)

-- | Merge L/R pair

reduceStep  ::  {-<-}(ProdObj con, forall α β. (con α, con β) => con (α,β), con (), con a, con (Prod xs)) =>{->-} Merge cat {-<-}con{->-} a xs ->
                (  forall zs. {-<-}con (Prod zs) => {->-}FreeSMC cat {-<-}con{->-} (Prod zs) (Prod xs)  ->
                  Merge cat {-<-}con{->-} a zs -> k) -> k
-- There exists at least one pair of the form L :<: f and R :<: f if
-- already maximally exposed. So we do not handle the base cases here.

-- If R :<: f is the first in the order, then L :<: f also exists;
-- and it should be first, so the case (R :<: f) :+ (L :<: g) must be rejected.
reduceStep ((P1 :<: f₁) :+ (P2 :<: f₂) :+ rest) k
  | EQ <- compareMorphisms f₁ f₂ =
  expose f₁               $ \g f' -> -- expose any merge
  appendSorted f' rest    $ \g' rest' -> -- insert the exposed stuff in a sorted way
  k (assoc ∘ (g × id) ∘ g') rest'
reduceStep (f :+ rest) k =
  reduceStep rest                    $ \g rest' ->
  appendSorted (f :+ Nil) rest'  $ \g' rest'' ->
  k ((unitor' × g) ∘ g') rest''
  
appendSorted  :: {-<-}(ProdObj con, forall x y. (con x, con y) => con (x,y), con (), con a, con (Prod xs), con (Prod ys)) => {->-} Merge cat {-<-}con{->-} a xs -> Merge cat {-<-}con{->-} a ys ->
                 (  forall zs. {-<-}con(Prod zs)=> {->-}FreeSMC cat {-<-}con{->-}  (Prod zs)
                                                                                   (Prod xs ⊗ Prod ys)  ->
                    Merge cat{-<-}con{->-} a zs -> k) -> k
appendSorted  Nil        ys         k = k (swap ∘  unitor)  ys
appendSorted  xs         Nil        k = k          unitor   xs
appendSorted  (x :+ xs)  (y :+ ys)  k =
  case compareMorphisms x y of
    GT  ->   appendSorted (x :+ xs) ys $ \a zs ->
             k (assoc ∘ (swap × id) ∘  assoc' ∘ (id × a))  (y :+ zs)
    _   ->   appendSorted xs (y :+ ys) $ \a zs ->
             k (                       assoc' ∘ (id × a))  (x :+ zs)

-- | intermediate result
data R cat con a b where
  St :: con (Prod b)
    => FreeSMC k con (Prod b) c -- already processed part (SMC here)
    -> Merge k con a b -- maximally exposed and sorted merge tree (see 'expose' below)
    -> R k con a c

-- | Perform 1 reduction step, assumes input is already maximally exposed and sorted. 
reductionStep :: (ProdObj con, forall α β. (con α, con β) => con (α,β), con (), con a, con b) => R cat con a b -> R cat con a b
reductionStep (St r1 (f :+ Nil)) = expose f $ \ready m -> St (r1 . unitor . ready) m  -- single morphism to analyse
reductionStep (St r1 m) = reduceStep m $ \r2 m' -> St (r1 . r2) m' -- L/R pair to find and reduce

-- | Perform all reduction steps and return intermediate states
reductionSteps  :: (ProdObj con, forall α β. (con α, con β) => con (α,β), con (), 
               con a, con b) => R cat con a b -> [R cat con a b]
reductionSteps st@(St _ Nil) = [st] -- done; and all the input is discarded
reductionSteps st@(St _ (I :+ Nil)) = [st] -- done!
reductionSteps st = st : reductionSteps (reductionStep st)

freeToR :: (ProdObj con, forall α β. (con α, con β) => con (α,β), con (),
           con x) => Cat k con a x -> R k con a x
freeToR f = St unitor' (f :+ Nil)

rToFree :: (Obj cat ~ con, ProdObj con, forall α β. (con α, con β) => con (α,β), con (), con a, con b)
        => R cat con a b -> FreeSMC cat con a b
rToFree (St done Nil) = unsafeCoerce done
-- why is the above safe? (St _ Nil) means that the whole input is
-- discarded/the whole expression has no free variable. In other
-- words, the input is the unit type. So we can coerce. 
--
-- Other explanation:
-- Our input is (f id), with f : (r `k` a) ⊸ r `k` b
-- We know that (f id) starts with discard.  Because f is linear, (f
-- id) can discard its input only if the input type is the unit in the
-- first place, hence a=().
rToFree (St done (I :+ Nil)) = done . unitor

reduce  :: (Obj cat ~ con, ProdObj con, forall α β. (con α, con β) => con (α,β), con (),
             con a, con b) => Cartesian.Cat cat con a b -> FreeSMC cat con a b
reduce = rToFree . last . reductionSteps . freeToR

-- Invariant: same source!
compareMorphisms :: (con a, con b, con c) => Cat cat con a b -> Cat cat con a c -> Order b c
compareMorphisms I I = EQ
compareMorphisms I _ = LT
compareMorphisms _ I = GT
compareMorphisms (f Cartesian.:>: g) (f' Cartesian.:>: g') =
  case compareAtoms g g' of
    LT -> LT
    GT -> GT
    EQ -> compareMorphisms f f'

-- Invariant: same source!
compareAtoms :: (con a, con b, con c) => Cat cat con a b -> Cat cat con a c -> Order b c
compareAtoms P1 P1 = EQ
compareAtoms P2 P2 = EQ
compareAtoms E E = EQ
compareAtoms (Embed _) (Embed _) = unsafeCoerce EQ -- Same source -> same Atoms
compareAtoms (f :▵: g) (f' :▵: g') = case compareMorphisms f f' of
  LT -> LT
  GT -> GT
  EQ -> case compareMorphisms g g' of
    LT -> LT
    GT -> GT
    EQ -> EQ
compareAtoms P1 _ = LT
compareAtoms _ P1 = GT
compareAtoms P2 _ = LT
compareAtoms _ P2 = GT
compareAtoms (Embed _) _ = LT
compareAtoms _ (Embed _) = GT
compareAtoms E _ = LT
compareAtoms _ E = GT
compareAtoms f g = error ("compareAtoms:\n" ++ showDbg 0 f "\n" ++ showDbg 0 g "" )

