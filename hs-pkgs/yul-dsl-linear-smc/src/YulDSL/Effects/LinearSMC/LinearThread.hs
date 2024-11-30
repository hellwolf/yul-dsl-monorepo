{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE UndecidableInstances #-}
module YulDSL.Effects.LinearSMC.LinearThread
  ( LTM (MkLTM), runLTM, startLTM, (>>=), (>>)
  , lift'l
  , fin'with, fin'dis, fin'emb, fin'const
  , sget, sput, sput_, sputAt
  ) where

-- base
import           GHC.TypeLits                            (type (+), type (<=))
-- linear-base
import           Control.Category.Linear
import           Prelude.Linear
import qualified Unsafe.Linear                           as UnsafeLinear
-- yul-dsl
import           YulDSL.Core
--
import           YulDSL.Effects.LinearSMC.LinearPipeline
import           YulDSL.Effects.LinearSMC.LinearPort


-- | Linear-thread Token.
data LTT (v :: Nat) = MkLTT

instance Consumable (LTT v) where
  consume = UnsafeLinear.coerce

-- | Linear thread graded monad.
newtype LTM v a = MkLTM (LTT v ⊸ a)

runLTM :: forall v a. LTM v a ⊸ LTT v ⊸ a
runLTM (MkLTM fa) = fa

startLTM :: forall a v. LTM v a ⊸ a
startLTM m = runLTM m MkLTT

-- | Monad bind operator for working with the QualifiedDo syntax.
(>>=) :: forall a va b vb. ()
      => LTM va a ⊸ (a ⊸ LTM vb b) ⊸ LTM vb b
(MkLTM fa) >>= f = MkLTM \lt -> runLTM (f (fa MkLTT)) lt

-- | monad discarding operator for working with the QualifiedDo syntax.
(>>) :: forall a va b vb. (Consumable a)
     => LTM va a ⊸ LTM vb b ⊸ LTM vb b
(MkLTM fa) >> mb = MkLTM \lt -> lseq (fa MkLTT) (runLTM mb lt)

infixl 1 >>=, >>

-- | Alternatively, you can use the LinearPipeLine to work with 'LTM'.
instance forall a b va vb. LinearPipelinePlus (LTM va a) (LTM vb b) (a ⊸ LTM vb b) where
  (&+) = (>>=)
-- | Alternatively, you can use the LinearPipeLine to work with 'LTM'.
instance forall a b va vb. ( Consumable a
                           ) => LinearPipelineMinus (LTM va a) (LTM vb b) (LTM vb b) where
  (&-) = (>>)

-- instance forall v a. LinearPipelineMinus (LTM v a) ((LT v ⊸ a) ⊸ a) where
--   (MkLTM (lt, a)) &- f = f lt a

-- -- | Run a linear thread.
-- runLT :: forall v r a. YulO2 a r => (LT 0 ⊸ P'V v r a) ⊸ P'V v r a
-- runLT f = f MkLTT

-- | Safe operation that lifts a pure yul port to a versioned yul port.
lift'l :: forall a r v. YulO2 a r
       => P'P r a ⊸ LTM v (P'V v r a)
lift'l x = MkLTM \lt -> lseq lt (UnsafeLinear.coerce x)

fin'with :: forall v r a. YulO2 a r
         => P'V v r a ⊸ LTM v (P'V v r a)
fin'with x = MkLTM \lt -> lseq lt x

fin'dis :: forall v r a. YulO2 a r
        => P'V v r a ⊸ LTM v (P'V v r ())
fin'dis x = MkLTM \lt -> lseq lt (discard x)

fin'emb :: forall v r a b. YulO3 a b r
        => a -> (P'V v r b ⊸ LTM v (P'V v r a))
fin'emb a b = MkLTM \lt -> lseq lt (emb'l a b)

fin'const :: forall v r a b. YulO3 a b r
          => P'V v r a -> (P'V v r b ⊸ LTM v (P'V v r a))
fin'const a b = MkLTM \lt -> lseq lt (const'l a b)

sget :: forall a r v. (YulO2 a r, ABIWordValue a)
     => P'V v r ADDR ⊸ LTM v (P'V v r a)
sget a = MkLTM \lt -> lseq lt (encode YulSGet a)

sput :: forall a r v. (YulO2 a r, ABIWordValue a)
     => P'V v r ADDR ⊸ P'V v r a ⊸ LTM (v + 1) (P'V (v + 1) r a)
sput to x =
  dup2'l x
  & \(x1, x2) -> encode YulSPut (merge (to, x1))
  & \u -> MkLTM \lt -> lseq lt (UnsafeLinear.coerce (ignore u x2))


sput_ :: forall a r v. (YulO2 a r, ABIWordValue a)
      => P'V v r ADDR ⊸ P'V v r a ⊸ LTM (v + 1) (P'V (v + 1) r ())
sput_ to x = encode YulSPut (merge (to, x))
             & \u -> MkLTM \lt -> lseq lt (UnsafeLinear.coerce u)

sputAt :: forall a r v. (YulO2 a r, ABIWordValue a)
       => ADDR ⊸ P'V v r a ⊸ LTM (v + 1) (P'V (v + 1) r a)
sputAt to x = mkUnit x & \(x', u) -> emb'l to u & \a -> sput a x'
