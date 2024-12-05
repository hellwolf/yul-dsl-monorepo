module Control.LinearlyVersionedMonad.Combinators
  (-- $lvm_combinators
    pure
  , embed -- embedN
  , toss -- tossN
  , passN, pass, passN_, pass_
  , with, with_, -- withN, withN_
  ) where

-- constraints
import           Data.Constraint.Linear         (Dict (Dict))
--
import           Control.LinearlyVersionedMonad
import           Data.LinearContext


-- $lvm_combinators
-- == Common Combinators for LVM
--
-- These combinators should be reexported by the contextualized LVM.

--------------------------------------------------------------------------------
-- pure
--------------------------------------------------------------------------------

-- | Lift a value in to the linearly versioned monad.
pure :: forall ctx v a. a ⊸ LVM ctx v v a
pure a = MkLVM (Dict, , a)

--------------------------------------------------------------------------------
-- embed
--------------------------------------------------------------------------------

-- | Embed a value in to the context of linearly versioned monad.
embed :: forall ctx v a m. ContextualEmbeddable ctx m a => a ⊸ LVM ctx v v (m a)
embed a = MkLVM \ctx -> let !(ctx', ma) = contextualEmbed ctx a in (Dict, ctx', ma)

--------------------------------------------------------------------------------
-- toss
--------------------------------------------------------------------------------

toss :: forall ctx v a. ContextualConsumable ctx a
     => a ⊸ LVM ctx v v ()
toss x = MkLVM \ctx -> (Dict, contextualConsume ctx x, ())

--------------------------------------------------------------------------------
-- pass
--------------------------------------------------------------------------------

-- | Single value version of 'passN'.
pass :: forall ctx va vb a b. (ContextualDupable ctx a)
     => a ⊸ (a ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb (a, b)
pass a mb = MkLVM \ctx ->
  let !(ctx', (a1, a2)) = contextualDup ctx a
      !(alteb, ctx'', b) = unLVM (mb a1) ctx'
  in (alteb, ctx'', (a2, b))

-- | Single value version of 'passN_'.
pass_ :: forall ctx va vb a b. (ContextualDupable ctx a, ContextualConsumable ctx b)
     => a ⊸ (a ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb a
pass_ a mb = MkLVM \ctx ->
  let !(ctx', (a1, a2)) = contextualDup ctx a
      !(alteb, ctx'', b) = unLVM (mb a1) ctx'
      ctx''' = contextualConsume ctx'' b
  in (alteb, ctx''', a2)

-- | Pass the copied data to the next process, then pass both the original data and the result to the next stage.
passN :: forall ctx va vb tpl b. ContextualDupableTupleN ctx tpl
      => tpl ⊸ (tpl ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb (tpl, b)
passN tpl mb = MkLVM \ctx ->
  let !(ctx', (tpl1, tpl2)) = contextualDupTupleN ctx tpl
      !(alteb, ctx'', b) = unLVM (mb tpl1) ctx'
  in (alteb, ctx'', (tpl2, b))

passN_ :: forall ctx va vb tpl b. (ContextualDupableTupleN ctx tpl, ContextualConsumable ctx b)
       => tpl ⊸ (tpl ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb tpl
passN_ tpl mb = passN tpl mb >>= (\(tpl', b) -> toss b >> pure tpl')

--------------------------------------------------------------------------------
-- with
--------------------------------------------------------------------------------

-- | Process with provided data without creating a copy of the data.
with :: forall ctx va vb a b. ()
     => a ⊸ (a ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb b
with a mb = MkLVM \ctx ->
  let !(alteb, ctx', b) = unLVM (mb a) ctx
  in (alteb, ctx', b)

-- | Process with provided data without creating a copy of the data.
with_ :: forall ctx va vb tpl b. (ContextualConsumable ctx b)
       => tpl ⊸ (tpl ⊸ LVM ctx va vb b) ⊸ LVM ctx va vb ()
with_ tpl mb = MkLVM \ctx ->
  let !(alteb, ctx', b) = unLVM (mb tpl) ctx
      ctx'' = contextualConsume ctx' b
  in (alteb, ctx'', ())
