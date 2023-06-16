{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-overlapping-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LinearTypes #-}

module Control.Category.FreeCartesian where

import Prelude hiding ((.),id,curry)
import Control.Category.Constrained
import Data.Kind

instance (forall x y. (con x, con y) => Show (k x y)) => Show  (Cat k con a b) where
  show x = showsPrec (-1) x ""
  showsPrec d = \case
    I -> showString "id"
    P1 -> showString "π₁"
    P2 -> showString "π₂"
    Embed s -> showString (show s)
    f :.: g -> showParen (d >  0) (showsPrec 0 f . showString " ∘ " . showsPrec 0 g)
    f :▵: g -> showParen (d > -1) (showsPrec 2 f . showString " ▵ " . showsPrec 2 g)

showDbg :: Int -> Cat k con a b -> ShowS
showDbg d = \case
    Embed _ -> showString "?"
    I -> showString "id"
    f :.: g -> showParen (d /= 0) (showDbg 0 f . showString " ∘ " . showDbg 0 g)
    f :▵: g -> showParen True (showDbg 2 f . showString " ▵ " . showDbg 2 g)
    P2 -> showString "π₂"
    P1 -> showString "π₁"


parens :: [Char] -> [Char]
parens x = "(" <> x <> ")"

mapGenerators :: (con a, con b) => (forall x y. (con x, con y) => k x y -> k' x y) -> Cat k con a b -> Cat k' con a b
mapGenerators f = \case
  I -> I
  Embed g -> Embed (f g)
  a :.: b -> mapGenerators f a :.: mapGenerators f b
  P1 -> P1
  P2 -> P2
  a :▵: b -> mapGenerators f a :▵: mapGenerators f b
  x -> error (showDbg 0 x " (Free.mapGenerators)")

type Cat = FreeCartesian

data FreeCartesian k {-<-} (con :: Type -> Constraint) {->-} a b where
  I      :: FreeCartesian k {-<-}con{->-} a a
  (:.:)  :: {-<-}con b => {->-} FreeCartesian k {-<-}con{->-} b c -> FreeCartesian k {-<-}con{->-} a b
         -> FreeCartesian k {-<-}con{->-} a c
  Embed  :: {-<-}(con a, con b) => {->-}k a b -> FreeCartesian k {-<-}con{->-} a b
  (:▵:)  :: {-<-}(con a, con b, con c) => {->-}FreeCartesian k {-<-}con {->-}a b -> FreeCartesian k {-<-}con{->-} a c
         -> FreeCartesian k {-<-}con{->-} a (b ⊗ c)
  P1     :: {-<-}con b => {->-} FreeCartesian k {-<-}con{->-} (a ⊗ b) a
  P2     :: {-<-}con a => {->-} FreeCartesian k {-<-}con{->-} (a ⊗ b) b {-<-}

assocRight :: (Cat k obj x y) -> (Cat k obj x y)
assocRight (a :.: (assocRight -> (b :.: c))) = (a :.: b) :.: c
assocRight x = x

rightView :: (obj a, obj c) => (Cat k obj a c) -> Cat k obj a c
rightView (assocRight -> (a :.: b)) = a :.: b
rightView x = I :.: x

assocLeft :: (Cat k obj x y) -> (Cat k obj x y)
assocLeft ((assocLeft -> (a :.: b)) :.: c) = a :.: (b :.: c)
assocLeft x = x

leftView :: (obj a, obj c) => (Cat k obj a c) -> Cat k obj a c
leftView (assocLeft -> (a :.: b)) = a :.: b
leftView x = x :.: I

pattern (:>:) ::  (obj x, obj y) => (obj b)  => Cat k obj b y -> Cat k obj x b -> Cat k obj x y
pattern f :>: g <- (rightView -> f :.: g)
  where f :>: g = f . g

pattern (:<:) ::  (obj x, obj y) => (obj b) => (Cat k obj b y) -> (Cat k obj x b) -> Cat k obj x y
pattern f :<: g <- (leftView -> f :.: g)
  where f :<: g = f . g

evalCartesian :: forall k a b con f.
              (ProdObj con, forall x y. (con x, con y) => con (x,y), con (),
               con ~ Obj k, Obj k a, Obj k b, Cartesian f, Obj f ~ con) =>
              (forall α β. (con α, con β) => k α β -> f α β)  ->
              Cat k (Obj k) a b -> f a b
evalCartesian embed = \case
  I -> id
  (f :.: g) -> evalCartesian embed f . evalCartesian embed g
  (Embed φ) -> embed φ
  P1 -> exl
  P2 -> exr
  f :▵: g -> evalCartesian embed f ▵ evalCartesian embed g
  

instance Category (Cat k con) where
  type Obj (Cat k con) = con
  id = I
  I ∘ x = x
  x ∘ I = x
  P1 ∘ (f :▵: _) = f
  P2 ∘ (_ :▵: g) = g
  x ∘ y = x :.: y
 

instance ({-<-}ProdObj con, con (), forall a b. (con a, con b) => con (a,b), {->-}Monoidal k) =>  Monoidal (FreeCartesian k {-<-}con{->-}) {-<-}where
  f × g = cartesianCross f g
  assoc = cartesianAssoc
  assoc' = cartesianAssoc'
  swap = cartesianSwap
  unitor = cartesianUnitor
  unitor' = cartesianUnitor'{->-}
instance ({-<-}ProdObj con, con (), forall a b. (con a, con b) => con (a,b),{->-} Monoidal k) => Cartesian (FreeCartesian k {-<-}con{->-}) {-<-}where
  exl = P1
  exr = P2
  dup = id :▵: id
  (▵) = (:▵:){->-}
