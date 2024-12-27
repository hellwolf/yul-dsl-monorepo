module YulDSL.Effects.LinearSMC.Storage
  ( sget, sput, sput_, sputAt
  ) where
-- base
import GHC.TypeLits                      (type (+))
-- constraints
import Data.Constraint.Unsafe            (unsafeAxiom)
-- linear-base
import Control.Category.Linear
import Prelude.Linear                    ((&))
import Unsafe.Linear                     qualified as UnsafeLinear
-- yul-dsl
import YulDSL.Core
-- linearly-versioned-monad
import Control.LinearlyVersionedMonad    (LVM (MkLVM))
--
import YulDSL.Effects.LinearSMC.YulMonad
import YulDSL.Effects.LinearSMC.YulPort


sget :: forall v r a. (YulO2 r a, ABIWordValue a)
     => P'V v r ADDR ⊸ YulMonad v v r (P'V v r a)
sget a = pure (encode YulSGet a)

sput :: forall v r a. (YulO2 r a, ABIWordValue a)
     => P'V v r ADDR ⊸ P'V v r a ⊸ YulMonad v (v + 1) r (P'V (v + 1) r a)
sput to x =
  dup2'l x
  & \(x1, x2) -> encode YulSPut (merge (to, x1))
  & \u -> MkLVM (unsafeAxiom, , UnsafeLinear.coerce (ignore u x2))

sput_ :: forall v r a. (YulO2 r a, ABIWordValue a)
      => P'V v r ADDR ⊸ P'V v r a ⊸ YulMonad v (v + 1) r (P'V (v + 1) r ())
sput_ to x =
  encode YulSPut (merge (to, x))
  & \u -> MkLVM (unsafeAxiom, , UnsafeLinear.coerce u)

sputAt :: forall v r a. (YulO2 r a, ABIWordValue a)
       => ADDR ⊸ P'V v r a ⊸ YulMonad v (v + 1) r (P'V (v + 1) r a)
sputAt to x = mkUnit x & \(x', u) -> emb'l to u & \a -> sput a x'
