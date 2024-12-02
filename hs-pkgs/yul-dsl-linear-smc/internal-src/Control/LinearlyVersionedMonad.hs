{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : LGPL-3

Maintainer  : hellwolf@yolc.dev
Stability   : experimental
Portability : GHC2024

= Description

Linearly versioned monad (LVM) provides a form of "linear safety" in handling effectful data operations.

A short description of such form of "linear safety" can be found in the documentation of 'LVM'.

This module is designed to work with the QualifiedDo syntax, where when import qualified as "LVM", an expression of
@LVM.do@ can start a do-notation syntax block where statements are de-sugared into'(LVM.>>=)' and '(LVM.>>)'.

There may be something to be said about the difference between parameterized and graded monad. In author's limited
understanding of this subject, it seems more appropriate to call 'LVM' parameterized only. For more study about this
matter, please refer to [Orchard, Dominic, Philip Wadler, and Harley Eades III. "Unifying graded and parameterised
monads." arXiv preprint arXiv:2001.10274 (2020).]

-}
module Control.LinearlyVersionedMonad
  ( -- $linear_safety
    LVM (MkLVM), unLVM
  , runLVM, (>>=), (>>)
    -- $lvm_combinators
  , pure
  , pass, with
  ) where

-- base
import           GHC.TypeLits           (Nat, type (<=))
-- constraints
import           Data.Constraint        (Dict (Dict), HasDict, withDict)
import           Data.Constraint.Nat    (leTrans)
-- linear-base
import qualified Control.Functor.Linear
import qualified Data.Functor.Linear
import           Prelude.Linear         (flip, lseq)
import qualified Unsafe.Linear          as UnsafeLinear
--
import           Data.LinearContext


-- $linear_safety
-- == Linear Safety Through Data Versioning
--
-- 'LVM' is parameterized with context data of type @ctx@, an input version @va@, and an output version @vb@. Given the
-- context, similar to the reader monad, it produces an output of type @a@ and a linearly updated new context.  More
-- importantly, it carries a proof that @va <= vb@. Such a proof is vital to provide the linear safety together with the
-- additional 'LVM' monad laws in relations to the bind operations '(>>=)', '(>>)'.

-- | Linear versioned monad (LVM) is a parameterized reader monad with linear safety.
newtype LVM ctx (va :: Nat) (vb :: Nat) a = MkLVM (ctx ⊸ (Dict (va <= vb), ctx, a))

-- | Unwrap the LVM linearly; otherwise the GHC default syntax createwith a multiplicity-polymorphic arrow.
unLVM :: forall ctx va vb a. LVM ctx va vb a ⊸ ctx ⊸ (Dict (va <= vb), ctx, a)
unLVM (MkLVM fa) = fa

-- | Run a linearly versioned monad.
runLVM :: forall a va vb ctx. ctx ⊸ LVM ctx va vb a ⊸ (ctx, a)
runLVM ctx m = let !(lp, ctx', a) = unLVM m ctx in discard_dict lp (ctx', a)

-- | Monad bind operator for working with the QualifiedDo syntax.
--
-- _Law of Monad_
--
-- 1) Left identity:  @ pure a \>>= h ≡ h a @
--
-- 2) Right identity: @ m \>>= pure ≡ m @
--
-- 3) Associativity:  @ (m \>>= g) \>>= h ≡ m \>>= (\x -> g x \>>= h) @
--
-- _Additional Law of Linearly Versioned Monad_
--
-- Additionally, each 'LVM' carries a proof of monotonic growth of data versions, denoted as such @m [va \<= vb]@. Then
-- we have:
--
-- 1) Law of linearly versioned monad: @ ma [va \<= vb] \>>= mb [vb <= vc] ≡ mc [va <= vc] @
(>>=) :: forall ctx va vb vc a b. ()
      => LVM ctx va vb a ⊸ (a ⊸ LVM ctx vb vc b) ⊸ LVM ctx va vc b
ma >>= f = MkLVM \ctx -> let !(alteb, ctx', a) = unLVM ma ctx
                             !(bltec, ctx'', a') = unLVM (f a) ctx'
                         in  (Dict \\ leTrans @va @vb @vc \\ alteb \\ bltec, ctx'', a')

-- | Monad discard operator for working with the QualifiedDo syntax.
(>>) :: forall ctx va vb vc a b. (ContextualConsumable ctx a)
     => LVM ctx va vb a ⊸ LVM ctx vb vc b ⊸ LVM ctx va vc b
ma >> mb = ma >>= \a -> MkLVM \ctx -> let !(bltec, ctx', b) = unLVM mb ctx
                                      in (bltec, contextualConsume ctx' a, b)

infixl 1 >>=, >>

-- $lvm_combinators
-- == Common Combinators for LVM
--
-- These combinators should be reexported by the contextualized LVM.

pure :: forall ctx v a. a ⊸ LVM ctx v v a
pure a = MkLVM \ctx -> (Dict, ctx, a)

-- | Pass the copied data to the next process, then pass both the original data and the result to the next stage.
pass :: forall ctx va vb a b. (ContextualDupable ctx a)
     => a ⊸ (a ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb (a, b)
pass a mb = MkLVM \ctx ->
  let !(ctx', (a1, a2)) = contextualDup ctx a
      !(alteb, ctx'', b) = unLVM (mb a1) ctx'
  in (alteb, ctx'', (a2, b))

-- | Process with provided data without creating a copy of the data.
with :: forall ctx va vb a b. ()
     => a ⊸ (a ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb b
with a mb = MkLVM \ctx ->
  let !(alteb, ctx', b) = unLVM (mb a) ctx
  in (alteb, ctx', b)

--
-- Instances for linear-base
--

instance Data.Functor.Linear.Functor (LVM ctx va vb) where
  fmap f ma = ma >>= \a -> MkLVM \ctx -> (Dict, ctx, f a)
instance Control.Functor.Linear.Functor (LVM ctx va vb) where
  fmap = UnsafeLinear.toLinear Data.Functor.Linear.fmap

--
-- Some unsafe internal functions for handling proofs
--

-- Discard the constraint proof linearly.
discard_dict :: Dict p ⊸ a ⊸ a
discard_dict p = lseq (UnsafeLinear.coerce p :: ())

-- Linear version of (\\) for internal use.
(\\) :: HasDict c e => (c => r) ⊸ e ⊸ r
(\\) = flip (UnsafeLinear.toLinear2 (withDict))
infixl 1 \\
