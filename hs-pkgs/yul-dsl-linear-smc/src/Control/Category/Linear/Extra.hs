{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.Category.Linear.Extra where

-- linear-base
import           Prelude.Linear
-- linear-smc
import           Control.Category.Constrained (Cartesian, Category (Obj), O3, O4, type (⊗))
import           Control.Category.Linear      (P, copy, merge, split)


copyAp :: forall k con r a b c.
          ( Cartesian k {-<-}, O4 k r a b c, con ~ Obj k {->-}
          , con (), (forall α β. (con α, con β) => con (α,β))
          ) => P k r a ⊸ (P k r a ⊸ P k r b) ⊸ (P k r a ⊸ P k r c) ⊸ P k r (b⊗c)
copyAp a f1 f2 = copy a & split & \(a1, a2) -> merge (f1 a1, f2 a2)

copyAp' :: forall k con r a b c.
           ( Cartesian k {-<-}, O4 k r a b c, con ~ Obj k {->-}
           , con (), (forall α β. (con α, con β) => con (α,β))
           ) => P k r a ⊸ (P k r a ⊸ P k r b) ⊸ (P k r a ⊸ P k r c) ⊸ (P k r b, P k r c)
copyAp' a f1 f2 = copy a & split & \(a1, a2) -> (f1 a1, f2 a2)

passAp :: forall k con r a b.
          ( Cartesian k {-<-}, O3 k r a b, con ~ Obj k {->-}
          , con (), (forall α β. (con α, con β) => con (α,β))
          ) => P k r a ⊸ (P k r a ⊸ P k r b) ⊸ (P k r a, P k r b)
passAp i = copyAp' i id
