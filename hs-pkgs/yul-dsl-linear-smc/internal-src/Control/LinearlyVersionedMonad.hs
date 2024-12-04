{-# OPTIONS_GHC -Wno-orphans #-}
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
-- deepseq
import           Control.DeepSeq        (rnf)
-- linear-base
import qualified Control.Functor.Linear
import qualified Data.Functor.Linear
import           Prelude.Linear         (Consumable (consume), flip, lseq, type (~))
import qualified Unsafe.Linear          as UnsafeLinear
-- eth-abi
import           Data.SimpleNP          (NP)
import           Data.TupleN
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
runLVM ctx m = let !(lp, ctx', a) = unLVM m ctx in lseq lp (ctx', a)

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
ma >>= f = MkLVM \ctx -> let !(aleb, ctx', a) = unLVM ma ctx
                             !(blec, ctx'', a') = unLVM (f a) ctx'
                         in  (Dict \\ leTrans @va @vb @vc \\ aleb \\ blec, ctx'', a')

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

-- | Lift a value in to the linearly versioned data.
pure :: forall ctx v a. a ⊸ LVM ctx v v a
pure a = MkLVM \ctx -> (Dict, ctx, a)

-- | Pass the copied data to the next process, then pass both the original data and the result to the next stage.
passN :: forall ctx va vb tpl b.
         ( ContextualDupable ctx (TupleNtoNP (tpl))
         , NPtoTupleN (TupleNtoNP tpl) ~ tpl
         , FromTupleNtoNP tpl
         , FromNPtoTupleN (TupleNtoNP tpl)
         )
      => tpl ⊸ (tpl ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb (tpl, b)
passN tpl mb = MkLVM \ctx ->
  let !np = fromTupleNPtoNP tpl
      !(ctx', (np1, np2)) = contextualDup ctx np
      !tpl1 = fromNPtoTupleN np1
      !tpl2 = fromNPtoTupleN np2
      !(alteb, ctx'', b) = unLVM (mb tpl1) ctx'
  in (alteb, ctx'', (tpl2, b))

-- | Similar to 'passN' but for a single value.
pass :: forall ctx va vb a b. (ContextualDupable ctx (NP '[a]))
     => a ⊸ (a ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb (a, b)
pass a mb = passN (MkSolo a) (\(MkSolo a') -> mb a') >>= un_solo

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

instance Consumable (Dict p) where
  consume = UnsafeLinear.toLinear rnf

-- Linear version of (\\) for internal use.
(\\) :: HasDict c e => (c => r) ⊸ e ⊸ r
(\\) = flip (UnsafeLinear.toLinear2 (withDict))
infixl 1 \\

un_solo :: (Solo a, b) ⊸ LVM ctx v v (a, b)
un_solo (MkSolo a, b) = MkLVM \ctx -> (Dict, ctx, (a, b))
