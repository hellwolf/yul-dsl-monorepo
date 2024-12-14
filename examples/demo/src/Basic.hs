module Basic where

import qualified Control.LinearlyVersionedMonad as LVM
import           Prelude.YulDSL

-- | A function that takes one uint and store its value doubled at a fixed storage location.
foo1 = fn @(U256 -> U256) "foo1" $
  \x -> if x > 9000 then 0 else x

-- | A function takes two uints and store their sum at a fixed storage location then returns true.
--
--   Note: you can create any number of "unit" signals by adding '()' to the input list.
foo2 = fn @(Maybe U256 -> Maybe U256 -> U256) "foo2" $
  \x1 x2 -> match (x1 + x2) \case
    Just r  -> r
    Nothing -> 0

-- | A function takes two uints and store their sum at a fixed storage location then returns it.
foo3 = fn'l "foo3" $ yulmonad'lv @(U256 -> U256 -> (BOOL, U256)) \x1 x2 -> LVM.do
  val <- sputAt (constAddr 0xdeadbeef) (x1 + x2)
  (val, t) <- pass val (pure . emb'l true)
  pure $ merge (t, val)

-- | "rangeSum" implemented in a value function
rangeSum'v1 = fn @(U256 -> U256 -> U256 -> U256) "rangeSumV1"
  \from step until -> let j = from + step
                      in from + if j <= until
                                then call'p rangeSum'v1 j step until
                                else emb'p 0

-- | "rangeSum" implemented in a value function, and a locally scoped function
rangeSum'v2 = go
  where
    go = fn @(U256 -> U256 -> U256 -> U256) "rangeSumV2" \from step until ->
      let j = from + step
      in from + if j <=? until then call'p go j step until else emb'p 0

-- | Sum a range @[i..t]@ of numbers separated by a step number @s@ as a linear function.
--
-- FIXME: call the pure version of rangeSum instead
rangeSum'l = fn'l "rangeSumL" $
  uncurry'lv @(U256 -> U256 -> U256 -> U256)
  \from step until -> mkUnit from &
  \(from, u) -> dup2'l from &
  \(from, from') -> dup2'l step &
  \(step, step') -> dup2'l until &
  \(until, until') -> dup2'l (from + step) &
  \(j, j') -> from' + if j <= until
                      then call'l rangeSum'l j' step' until'
                      else emb'l 0 u

object = mkYulObject "Basic" emptyCtor
         [ externalFn foo1
         -- , externalFn foo2
         , staticFn   foo3 -- FIXME this should not be possible with permission tag
         , staticFn rangeSum'l
         , staticFn rangeSum'v1
         , staticFn rangeSum'v2
         -- , externalFn rangeSumVFn
         ]
