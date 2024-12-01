module Control.LinearVersionedMonad
  ( LVM (MkLVM), unLVM
  , runLVM, (>>=), (>>)
  , ContextualConsumable (contextualConsume)
  ) where

-- base
import           GHC.TypeLits        (Nat, type (<=))
-- constraints
import           Data.Constraint     (Dict (Dict), HasDict, withDict)
import           Data.Constraint.Nat (leTrans)
-- linear-base
import           Prelude.Linear      (Consumable (consume), flip, lseq)
import qualified Unsafe.Linear       as UnsafeLinear

-- | Linear-thread Token; thimust be guarded and inaccessible outside of thimodule.
data LT (v :: Nat) = MkLT

-- | Linear-thread Token shall not be movable (becoming unrestricted), but it can be consumed.
instance Consumable (LT v) where
  consume = UnsafeLinear.coerce

class ContextualConsumable ctx a where
  contextualConsume :: ctx -> a -> ctx

-- | Linear versioned monad (LVM).
newtype LVM ctx va vb a = MkLVM ((LT vb, ctx) ⊸ (Dict (va <= vb), ctx, a))

-- | Unwrap the LVM linearly; otherwise the GHC default syntax createwith a multiplicity-polymorphic arrow.
unLVM :: forall ctx va vb a. LVM ctx va vb a ⊸ (LT vb, ctx) ⊸ (Dict (va <= vb), ctx, a)
unLVM (MkLVM fa) = fa

-- | Start a linear versioned thread.
runLVM :: forall a va vb ctx. ctx ⊸ LVM ctx va vb a ⊸ (ctx, a)
runLVM ctx m = let !(lp, ctx', a) = unLVM m (MkLT, ctx) in discard_dict lp (ctx', a)

-- | Monad bind operator for working with the QualifiedDo syntax.
(>>=) :: forall ctx va vb vc a b. ()
      => LVM ctx va vb a ⊸ (a ⊸ LVM ctx vb vc b) ⊸ LVM ctx va vc b
(MkLVM fa) >>= f = MkLVM \(lt, ctx) -> let !(alteb, ctx', a) = fa (MkLT, ctx)
                                           !(bltec, ctx'', a') = unLVM (f a) (lt, ctx')
                                       in  (Dict \\ leTrans @va @vb @vc \\ alteb \\ bltec, ctx'', a')

-- | Monad discard operator for working with the QualifiedDo syntax.
(>>) :: forall ctx va vb vc a b. (ContextualConsumable ctx a)
     => LVM ctx va vb a ⊸ LVM ctx vb vc b ⊸ LVM ctx va vc b
ma >> mb = ma >>= \a -> MkLVM \(lt, ctx) -> let !(lp, ctx', b) = unLVM mb (lt, ctx)
                                            in (lp, contextualConsume ctx' a, b)

infixl 1 >>=, >>

--
-- Some internal function
--

-- Discard the constraint proof linearly.
discard_dict :: Dict p ⊸ a ⊸ a
discard_dict p = lseq (UnsafeLinear.coerce p :: ())

-- | Linear version of (\\) for internal use.
(\\) :: HasDict c e => (c => r) ⊸ e ⊸ r
(\\) = flip (UnsafeLinear.toLinear2 (withDict))
infixl 1 \\
