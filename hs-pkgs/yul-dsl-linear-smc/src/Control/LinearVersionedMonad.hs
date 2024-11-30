module Control.LinearVersionedMonad
  ( LVM (MkLVM), unLVM
  , runLVM, (>>=), (>>)
  ) where

-- base
import           GHC.TypeLits   (Nat, type (<=))
-- constraints
import Data.Constraint (Dict)
import           Data.Constraint.Unsafe           (unsafeAxiom)
-- linear-base
import           Prelude.Linear (Consumable (consume), lseq)
import qualified Unsafe.Linear  as UnsafeLinear


-- | Linear-thread Token; this must be guarded and inaccessible outside of this module.
data LT (v :: Nat) = MkLT

-- | Linear-thread Token shall not be movable (becoming unrestricted), but it can be consumed.
instance Consumable (LT v) where
  consume = UnsafeLinear.coerce

-- | Linear versioned monad (LVM).
newtype LVM s ctx va vb a = MkLVM ((LT vb, ctx) ⊸ (Dict (va <= vb), ctx, a))

-- Discard the constraint proof linearly.
discard_dict :: Dict p ⊸ a ⊸ a
discard_dict p = lseq (UnsafeLinear.coerce p :: ())

-- | Unwrap the LVM linearly; otherwise the GHC default syntax creates with a multiplicity-polymorphic arrow.
unLVM :: forall s ctx va vb a. LVM s ctx va vb a ⊸ (LT vb, ctx) ⊸ (Dict (va <= vb), ctx, a)
unLVM (MkLVM fa) = fa

-- | Start a linear versioned thread.
runLVM :: forall a va vb ctx. ctx -> (forall s. LVM s ctx va vb a) ⊸ (ctx, a)
runLVM ctx m = let %1 !(lp, ctx', a) = unLVM m (MkLT, ctx) in discard_dict lp (ctx', a)

-- | Monad bind operator for working with the QualifiedDo syntax.
(>>=) :: forall s ctx va vb vc a b. ()
      => LVM s ctx va vb a ⊸ (a ⊸ LVM s ctx vb vc b) ⊸ LVM s ctx va vc b
(MkLVM fa) >>= f = MkLVM \(lt, ctx) -> let !(lp, ctx', a) = fa (MkLT, ctx)
                                           !(lp', ctx'', a') = unLVM (f a) (lt, ctx')
                                           lp'' = discard_dict lp (discard_dict lp') unsafeAxiom
                                       in  (lp'', ctx'', a')

-- | Monad discard operator for working with the QualifiedDo syntax.
(>>) :: forall s ctx va vb vc a b. (Consumable a)
     => LVM s ctx va vb a ⊸ LVM s ctx vb vc b ⊸ LVM s ctx va vc b
ma >> mb = ma >>= (\a -> lseq a mb)

infixl 1 >>=, >>
