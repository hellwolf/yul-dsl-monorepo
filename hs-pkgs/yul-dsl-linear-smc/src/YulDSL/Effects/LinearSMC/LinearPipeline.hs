{-# LANGUAGE FunctionalDependencies #-}
module YulDSL.Effects.LinearSMC.LinearPipeline
  ( LinearPipelinePlus ((&+)), LinearPipelineMinus ((&-))
  , use'l, Use'L (..), Dis'L (..), dis'l --, dup'l
  ) where
-- base
-- linera-base
import           Control.Category.Linear             (copy, discard, ignore, split)
import           Prelude.Linear
-- yul-dsl
import           YulDSL.Core
--
import           YulDSL.Effects.LinearSMC.LinearPort

class LinearPipelinePlus a result expr | a result -> expr where
  (&+) :: forall. a ⊸ expr ⊸ result

class LinearPipelineMinus a result expr | a result -> expr where
  (&-) :: forall. a ⊸ expr ⊸ result

infixl 1 &-, &+

newtype Use'L a b r v = MkUse'L (P'V v r a, b)
use'l :: forall a b r v. (YulO2 a r)
      => P'V v r a ⊸ (P'V v r a ⊸ b) ⊸ Use'L a b r v
use'l a f = split (copy a) & \(a', a'') -> MkUse'L (a', f a'')
instance forall a b c r v.
         ( YulO2 a r
         ) => LinearPipelinePlus (Use'L a b r v) c (P'V v r a ⊸ b ⊸ c) where
  (MkUse'L (a, b)) &+ f = f a b
-- instance forall a b c r v.
--          ( YulO4 r a b c
--          ) => LinearPipelineMinus (Use'L a b r v) (P'V v r c) (P'V v r a ⊸ P'V v r c) where
--   (MkUse'L (a, b)) &- f = ignore (discard b) (f a)

newtype Dis'L r v = MkDis'L (P'V v r ())
dis'l :: forall a r v. YulO2 a r => P'V v r a ⊸ Dis'L r v
dis'l = MkDis'L . discard
instance forall r b v.
         ( YulO2 r b
         ) => LinearPipelinePlus (Dis'L r v) b (P'V v r () ⊸ b) where
  (MkDis'L u) &+ f = f u
instance forall r b v.
         ( YulO2 r b
         ) => LinearPipelineMinus (Dis'L r v) (P'V v r b) (P'V v r b) where
  (MkDis'L u) &- b = ignore u b

-- data Dup'L (n :: Nat) (eff :: PortEffect) r a where
--   MkDup'L :: forall n eff r a. P'x eff r a ⊸ Dup'L n eff r a
--
-- dup'l :: forall (n :: Nat) -> P'x eff r a ⊸ Dup'L n eff r a
-- dup'l n x = MkDup'L @n x
-- instance forall eff r a b.
--          ( YulO2 r a
--          ) => LinearPipelinePlus (Dup'L 1 eff r a)
--          ((P'x eff r a ⊸ P'x eff r b) ⊸ P'x eff r b) where
--   (MkDup'L x) &+ f = f x
-- instance forall n eff r a b g.
--          ( YulO2 r a
--          , LinearPipelinePlus (Dup'L (n - 1) eff r a) ((g ⊸ P'x eff r b) ⊸ P'x eff r b)
--          ) => LinearPipelinePlus (Dup'L n eff r a)
--          ((P'x eff r a ⊸ (g ⊸ P'x eff r b)) ⊸ P'x eff r b) where
