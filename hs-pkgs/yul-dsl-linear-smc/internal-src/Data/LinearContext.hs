module Data.LinearContext
  ( ContextualConsumable (..)
  , ContextualDupable (..)
  ) where

-- | Providing a linear context @ctx@ for consuming @a@.
class ContextualConsumable ctx a where
  -- | Consume @a@ linearly.
  contextualConsume :: ctx ⊸ a ⊸ ctx

-- | Providing a linear context @ctx@ for duplicate=ing @a@.
class ContextualDupable ctx a where
  -- | Duplicate @a@ linearly.
  contextualDup :: ctx ⊸ a ⊸ (ctx, (a, a))
