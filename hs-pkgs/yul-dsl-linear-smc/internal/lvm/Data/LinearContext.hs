module Data.LinearContext
  ( ContextualConsumable (contextualConsume)
  , ContextualDupable (contextualDup)
  , ContextualEmbeddable (contextualEmbed)
  , ContextualDupableTupleN, contextualDupTupleN
  ) where

-- linear-base
import           Prelude.Linear (lseq)
-- eth-abi
import           Data.SimpleNP  (NP (..))
import           Data.TupleN


-- | Providing a linear context @ctx@ for consuming @a@.
class ContextualConsumable ctx a where
  -- | Consume @a@ linearly.
  contextualConsume :: ctx ⊸ a ⊸ ctx

-- | Providing a linear context @ctx@ for duplicate=ing @a@.
class ContextualDupable ctx a where
  -- | Duplicate @a@ linearly.
  contextualDup :: ctx ⊸ a ⊸ (ctx, (a, a))

-- | Providing a linear context @ctx@ for embedding a pure value @a@ in @m@.
class ContextualEmbeddable ctx m a where
  -- | Consume @a@ linearly.
  contextualEmbed :: ctx ⊸ a ⊸ (ctx, m a)

instance ContextualConsumable ctx () where
  contextualConsume ctx x = lseq x ctx

instance ContextualConsumable ctx (NP '[]) where
  contextualConsume ctx Nil = ctx

instance ( ContextualConsumable ctx x
         , ContextualConsumable ctx (NP xs)
         ) => ContextualConsumable ctx (NP (x:xs)) where
  contextualConsume ctx (x :* xs) = let ctx' = contextualConsume ctx x
                                    in contextualConsume ctx' xs

instance ContextualDupable ctx (NP '[]) where
  contextualDup ctx Nil = (ctx, (Nil, Nil))

instance ( ContextualDupable ctx x
         , ContextualDupable ctx (NP xs)
         ) => ContextualDupable ctx (NP (x:xs)) where
  contextualDup ctx (x :* xs) = let !(ctx', (x', x'')) = contextualDup ctx x
                                    !(ctx'', (xs', xs'')) = contextualDup ctx' xs
                                in (ctx'', (x' :* xs', x'' :* xs''))

type ContextualDupableTupleN ctx tpl = ( ConvertibleTupleN tpl
                                       , ContextualDupable ctx (TupleNtoNP (tpl))
                                       )

contextualDupTupleN :: forall ctx tpl. ContextualDupableTupleN ctx tpl
                    => ctx ⊸ tpl ⊸ (ctx, (tpl, tpl))
contextualDupTupleN ctx tpl = let np = fromTupleNPtoNP tpl
                                  !(ctx', (np1, np2)) = contextualDup ctx np
                              in (ctx', (fromNPtoTupleN np1, fromNPtoTupleN np2))
