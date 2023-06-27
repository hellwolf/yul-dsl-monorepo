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


module Control.Category.Linear.Internal where

import Data.Kind (Type)

import Prelude hiding ((.),id,curry,LT,GT,EQ)
import Control.Category.Constrained
import Control.Category.FreeCartesian as FC
-- import Control.Category.Show as Show
-- import qualified Control.Category.InitialSMC as Init

-- Linear patterns don't really work, and some version of GHC do not accept this declaration at all. Disabled for now.
-- pattern (:::) :: forall con (k :: Type -> Type -> Type) r a b.
--                    (Obj k r, Obj k a, Obj k b, Monoidal k, con (), (forall α β. (con α, con β) => con (α,β)), con ~ Obj k) =>
--                    P k r a ⊸ P k r b ⊸ P k r (a, b)
-- pattern x ::: y <- (split @con -> (x,y))
--   where x ::: y = merge @con (x,y)

-- infixr ::: -- GHC does not always see this change. rm -r dist/dante. T_T (ghc 8.8.4)


type P :: (Type -> Type -> Type) -> Type -> Type -> Type



split    :: {-<-}forall con a b r k. (O3 k r a b, Monoidal k, con (), (forall α β. (con α, con β) => con (α,β)), con ~ Obj k) => {->-}P k r (a ⊗ b) ⊸ (P k r a, P k r b)
merge    :: {-<-}forall  con a b r k. (O3 k r a b, Monoidal k, con(), (forall α β. (con α, con β) => con (α,β)), con ~ Obj k) => {->-}(P k r a , P k r b) ⊸ P k r (a ⊗ b)
encode   :: {-<-} (O3 k r a b, TensorClosed con, con ~ Obj k) => {->-}  (a `k` b) -> (P k r a ⊸ P k r b)

(!:) :: forall  con a b r k. (O3 k r a b, Monoidal k, con(), (forall α β. (con α, con β) => con (α,β)), con ~ Obj k)
       => P k r a ⊸ P k r b ⊸ P k r (a,b)
x !: y = merge (x,y)


data P k r a     where
  Y :: Cat k {-<-} (Obj k) {->-} r a -> P k r a
fromP :: P k r a -> Cat k {-<-} (Obj k) {->-} r a
fromP (Y f) = f

  
encode φ (Y f)    = Y (f ∘ embed φ) -- put φ after f.
split (Y f)       = (Y (f ∘ π1), Y (f ∘ π2)) 
merge (Y f, Y g)  = Y (f ▴ g)

decode   :: {-<-} forall a b k con. (con (), con ~ Obj k, Monoidal k, con a, con b, (forall α β. (con α, con β) => con (α,β))) => {->-} (forall r. {-<-}Obj k r =>{->-} P k r a ⊸ P k r b) -> (a `k` b)
decode f          = toSMC (extract f)

extract           :: {-<-} (Obj k a, Obj k b) => {->-} (forall r. {-<-} Obj k r => {->-} P k r a ⊸ P k r b) -> Cat k {-<-} (Obj k) {->-} a b
extract f         = fromP (f (Y id))

ignore      :: (Monoidal k, {-<-} O3 k r a (), (forall α β. (con α, con β) => con (α,β)), con ~ Obj k {->-}) => P k r () ⊸ P k r a ⊸ P k r a
ignore f g  = encode unitor' (merge (g,f))

mkUnit   :: {-<-}forall k con a r. (Obj k r, Monoidal k, con (), con a, (forall α β. (con α, con β) => con (α,β)), con ~ Obj k)         => {->-}P k r a ⊸ (P k r a, P k r ())
mkUnit x = split (encode unitor x) 

---------------------------------------------------------------------
-- If the underlying category is cartesian, we have additionally:
  
copy  :: (Cartesian k {-<-} , O2 k r a, (forall α β. (con α, con β) => con (α,β)), con ~ Obj k {->-} ) => P k r a ⊸ P k r (a ⊗ a)
copy  = encode dup
discard  :: (Cartesian k {-<-} , O2 k r a, (forall α β. (con α, con β) => con (α,β)), con ~ Obj k, con () {->-} ) => P k r a ⊸ P k r ()
discard  = encode dis
