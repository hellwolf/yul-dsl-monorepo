{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Constraint.Linear
  ( module           Data.Constraint
  , (\\)
  ) where

-- constraints
import Data.Constraint hiding ((\\))
-- deepseq
import Control.DeepSeq (rnf)
-- linear-base
import Prelude.Linear  (Consumable (consume), flip)
import Unsafe.Linear   qualified as UnsafeLinear


-- Linear version of (\\) for internal use.
(\\) :: HasDict c e => (c => r) ⊸ e ⊸ r
(\\) = flip (UnsafeLinear.toLinear2 (withDict))
infixl 1 \\

instance Consumable (Dict p) where
  consume = UnsafeLinear.toLinear rnf
