{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Category.StructuredObject where

import Control.Category.Constrained
import Prelude hiding (id, (.))
import Data.Array (Ix)
import Data.Constraint (Dict(..),Constraint)

-- representation for the objects
data Repr s a where
  RUnit :: Repr s ()
  RPair :: Repr s a -> Repr s b -> Repr s (a,b)
  RAtom :: Repr s (Atom s)
  RDual :: Repr s a -> Repr s (Dual a)

-----------------
-- OO
class OO s a where
  type Sub s t a
  getRepr :: Repr s a
  subOO :: forall c b. (OO s a, a ~ (c ⊗ b)) => Dict (OO s c, OO s b)
  subOO = error "not a product"
  dualOO :: forall z. (OO s a, a ~ Dual z) => Dict (OO s z)
  dualOO = error "not a dual"
  toList :: a -> [s]

type Structured s con = forall x. con x => OO s x :: Constraint

type Basic s = (Bounded s, Enum s, Ix s)

instance OO s () where
  type Sub s t () = ()
  getRepr = RUnit
  toList () = []
instance Basic s => OO s (Atom s) where
  type Sub s t (Atom s) = Atom t
  getRepr = RAtom
  toList (Atom x) = [x]
instance OO s a => OO s (Dual a) where
  type Sub s t (Dual a) = Dual (Sub s t a)
  getRepr = RDual getRepr
  dualOO = Dict
  toList (Dual a) = toList a

mapRepr :: forall t s a. Repr s a -> Repr t (Sub s t a)
mapRepr RAtom = RAtom
mapRepr RUnit = RUnit
mapRepr (RPair q r) = RPair (mapRepr q) (mapRepr r)
mapRepr (RDual r) = RDual (mapRepr r)

instance (OO s a, OO s b) => OO s (a⊗b) where
  toList (a,b) = toList a <> toList b
  type Sub s t (a,b) = (Sub s t a,Sub s t b)
  getRepr = RPair getRepr getRepr
  subOO = Dict

instance ProdObj (OO s) where
  prodobj = Dict
  objprod = subOO
  objunit = Dict

instance AutonomousObj (OO s) where
  objDual = Dict
  dualObj :: forall z a con. (z ~ Dual a, con z, con ~ OO s) => Dict (con a)
  dualObj = dualOO @s @z



