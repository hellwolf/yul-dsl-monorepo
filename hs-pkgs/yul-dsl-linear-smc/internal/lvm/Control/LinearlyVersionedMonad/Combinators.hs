{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : LGPL-3

Maintainer  : hellwolf@yolc.dev
Stability   : experimental
Portability : GHC2024

= Description

These are a set of common combinators for linearly versioned monad.

These combinators should be reexported by the contextualized 'Control.LinearlyVersionedMonad.LVM'.

-}
module Control.LinearlyVersionedMonad.Combinators
  ( pure
  , embed
  , toss, tossN
  , pass, pass_, passN, passN_
  , with, with_
  ) where

-- constraints
import           Data.Constraint.Linear         (Dict (Dict))
--
import           Control.LinearlyVersionedMonad
import           Data.LinearContext
import           Data.TupleN                    (ConvertibleTupleN, TupleNtoNP, fromTupleNtoNP)

--------------------------------------------------------------------------------
-- pure
--------------------------------------------------------------------------------

-- | Lift a value in to a LVM.
pure :: forall ctx v a. a ⊸ LVM ctx v v a
pure a = MkLVM (Dict, , a)

--------------------------------------------------------------------------------
-- embed
--------------------------------------------------------------------------------

-- | Embed a value into the context of a LVM.
embed :: forall ctx v a m. ContextualEmbeddable ctx m a
      => a ⊸ LVM ctx v v (m a)
embed a = MkLVM \ctx -> let !(ctx', ma) = contextualEmbed ctx a in (Dict, ctx', ma)

--------------------------------------------------------------------------------
-- toss
--------------------------------------------------------------------------------

-- | Toss a single value into the context of a LVM.
toss :: forall ctx v a. ContextualConsumable ctx a
     => a ⊸ LVM ctx v v ()
toss x = MkLVM \ctx -> (Dict, contextualConsume ctx x, ())

-- | Toss a TupleN into the context of a LVM.
tossN :: forall ctx v tpl. (ConvertibleTupleN tpl, ContextualConsumable ctx (TupleNtoNP tpl))
     => tpl ⊸ LVM ctx v v ()
tossN tpl = MkLVM \ctx -> (Dict, contextualConsume ctx (fromTupleNtoNP tpl), ())

--------------------------------------------------------------------------------
-- pass
--------------------------------------------------------------------------------

-- | Pass the copied data to the next process, then pass both the original data and the result to the next stage.
pass :: forall ctx va vb a b. (ContextualDupable ctx a)
     => a ⊸ (a ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb (a, b)
pass a mb = MkLVM \ctx ->
  let !(ctx', (a1, a2)) = contextualDup ctx a
      !(alteb, ctx'', b) = unLVM (mb a1) ctx'
  in (alteb, ctx'', (a2, b))

-- | Pass the copied data to the next process, then pass the original data to the next stage and discard the restart.
pass_ :: forall ctx va vb a b. (ContextualDupable ctx a, ContextualConsumable ctx b)
     => a ⊸ (a ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb a
pass_ a mb = MkLVM \ctx ->
  let !(ctx', (a1, a2)) = contextualDup ctx a
      !(alteb, ctx'', b) = unLVM (mb a1) ctx'
      ctx''' = contextualConsume ctx'' b
  in (alteb, ctx''', a2)

-- | 'pass' for TupleN.
passN :: forall ctx va vb tpl b. (ConvertibleTupleN tpl, ContextualDupable ctx (TupleNtoNP tpl))
      => tpl ⊸ (tpl ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb (tpl, b)
passN tpl mb = MkLVM \ctx ->
  let !(ctx', (tpl1, tpl2)) = contextualDupTupleN ctx tpl
      !(alteb, ctx'', b) = unLVM (mb tpl1) ctx'
  in (alteb, ctx'', (tpl2, b))

-- | 'pass_' for TupleN.
passN_ :: forall ctx va vb tpl b.
          ( ConvertibleTupleN tpl, ContextualDupable ctx (TupleNtoNP tpl)
          , ContextualConsumable ctx b
          )
       => tpl ⊸ (tpl ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb tpl
passN_ tpl mb = passN tpl mb >>= (\(tpl', b) -> toss b >> pure tpl')

--------------------------------------------------------------------------------
-- with
--------------------------------------------------------------------------------

-- | Process input @a@ without creating a copy of it then return the result @b@.
with :: forall ctx va vb a b. ()
     => a ⊸ (a ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb b
with a mb = MkLVM \ctx ->
  let !(alteb, ctx', b) = unLVM (mb a) ctx
  in (alteb, ctx', b)

-- | Process input @a@ without creating a copy of it then discard the result.
with_ :: forall ctx va vb tpl b. (ContextualConsumable ctx b)
       => tpl ⊸ (tpl ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb ()
with_ tpl mb = MkLVM \ctx ->
  let !(alteb, ctx', b) = unLVM (mb tpl) ctx
      ctx'' = contextualConsume ctx' b
  in (alteb, ctx'', ())
