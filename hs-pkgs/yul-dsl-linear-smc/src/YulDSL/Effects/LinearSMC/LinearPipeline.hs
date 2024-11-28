{-# LANGUAGE UndecidableInstances #-}
module YulDSL.Effects.LinearSMC.LinearPipeline where
-- base
import           GHC.TypeLits                        (type (<=))
-- linera-base
import           Control.Category.Linear             (copy, discard, ignore, split)
import           Prelude.Linear
import qualified Unsafe.Linear                       as UnsafeLinear
-- yul-dsl
import           YulDSL.Core
--
import           YulDSL.Effects.LinearSMC.LinearPort

class ProcessLinePass a expr where
  (&+) :: forall. a ⊸ expr

class ProcessLineDiscard a expr where
  (&-) :: forall. a ⊸ expr

infixl 1 &-, &+

newtype Use'L a b r vn = MkUse'L (P'V vn r a, P'V vn r b)
use'l :: forall a b r v1 vn. (YulO3 a b r, v1 <= vn)
      => P'V v1 r a ⊸ (P'V v1 r a ⊸ P'V vn r b) ⊸ Use'L a b r vn
use'l a f = split (copy a) & \(a', a'') -> MkUse'L (UnsafeLinear.coerce a', f a'')
instance forall a b c r v1 vn.
         ( YulO4 r a b c
         , v1 <= vn
         ) => ProcessLinePass (Use'L a b r v1)
         ((P'V v1 r a ⊸ P'V v1 r b ⊸ P'V vn r c) ⊸ P'V vn r c) where
  (MkUse'L (a, b)) &+ f = f a b
instance forall a b c r v1 vn.
         ( YulO4 r a b c
         , v1 <= vn
         ) => ProcessLineDiscard (Use'L a b r v1)
         ((P'V v1 r a ⊸ P'V vn r c) ⊸ P'V vn r c) where
  (MkUse'L (a, b)) &- f = ignore (UnsafeLinear.coerce (discard b)) (f a)

newtype Dis'L r v = MkDis'L (P'V v r ())
dis'l :: forall a r v. YulO2 a r => P'V v r a ⊸ Dis'L r v
dis'l = MkDis'L . discard
instance forall r v1 vn b.
         ( YulO2 r b
         , v1 <= vn
         ) => ProcessLinePass (Dis'L r v1)
         ((P'V v1 r () ⊸ P'V vn r b) ⊸ P'V vn r b) where
  (MkDis'L u) &+ f = f u
instance forall b r v1 vn.
         ( YulO2 r b
         , v1 <= vn
         ) => ProcessLineDiscard (Dis'L r v1)
         (P'V vn r b ⊸ P'V vn r b) where
  (MkDis'L u) &- b = ignore (UnsafeLinear.coerce u) b

data Dup'L (n :: Nat) (eff :: PortEffect) r a where
  MkDup'L :: forall n eff r a. P'x eff r a ⊸ Dup'L n eff r a

dup'l :: forall (n :: Nat) -> P'x eff r a ⊸ Dup'L n eff r a
dup'l n x = MkDup'L @n x
instance forall eff r a b.
         ( YulO2 r a
         ) => ProcessLinePass (Dup'L 1 eff r a)
         ((P'x eff r a ⊸ P'x eff r b) ⊸ P'x eff r b) where
  (MkDup'L x) &+ f = f x
-- instance forall n eff r a b g.
--          ( YulO2 r a
--          , ProcessLinePass (Dup'L (n - 1) eff r a) ((g ⊸ P'x eff r b) ⊸ P'x eff r b)
--          ) => ProcessLinePass (Dup'L n eff r a)
--          ((P'x eff r a ⊸ (g ⊸ P'x eff r b)) ⊸ P'x eff r b) where
