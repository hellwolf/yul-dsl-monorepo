{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Basic where

------------------------------------------------------------------------------------------------------------------------
-- Trivial Diagrams
------------------------------------------------------------------------------------------------------------------------

simple_id = MkFnCat "simple_id" $ YulId @U256

simple_coerce = MkFnCat "simple_coerce" $ YulCoerce @U256 @U256

const_42 :: Fn (U256)
const_42 = MkFn (MkFnCat "const_42" $ decode (const'l (fromInteger 42)))

-- nop :: Fn () ()
-- nop = fn'l "nop" $ curry'l @(()) \(u) -> u

disFn :: ABITypeable a => Fn (a -> ())
disFn = MkFn (MkFnCat "disFn" YulDis)

-- mkConst :: forall a b. YulO2 a b => b -> Fn (a -> b)
-- mkConst b = fn'l "mkConst" $ curry'l @(a -> b) (\a -> const'l b a)

-- | A function that takes one uint and store its value doubled at a fixed storage location.
foo1 :: Fn (U256 -> U256)
foo1 = fn'l "foo1"
  (curry'l @(U256 -> U256) \x ->
      dup2'l x & \(x, x') -> x + x'
  )

-- | A function takes two uints and store their sum at a fixed storage location then returns true.
--
--   Note: you can create any number of "unit" signals by adding '()' to the input list.
foo2 :: Fn (U256 -> U256 -> U256)
foo2 = fn'l "foo2"
  ( curry'l @(U256 -> U256 -> U256)
    \x1 x2 -> dup2'l x2 &
    \(x2, x2') -> x1 + (x2 + x2')
  )

-- | A function takes two uints and store their sum at a fixed storage location then returns it.
foo3 :: Fn (U256 -> U256 -> () -> (BOOL, U256))
foo3 = fn'l "foo3"
  ( curry'l @(U256 -> U256 -> () -> (BOOL, U256))
    \x1 x2 u -> dup2'l (x1 + x2) &
    \(r, r') -> ignore (toAddr 0xdeadbeef <==@ r) (merge (const'l true u, r'))
  )

-- | Sum a range @[i..t]@ of numbers separated by a step number @s@ as a linear function.
rangeSum'l = fn'l "rangeSumLFn"
  ( curry'l @(U256 -> U256 -> U256 -> U256)
    \from step until -> mkUnit from &
    \(from, u) -> dup2'l from &
    \(from, from') -> dup2'l step &
    \(step, step') -> dup2'l until &
    \(until, until') -> dup2'l (from + step) &
    \(j, j') -> from' + if j <=? until
                        then call'l rangeSum'l j' step' until'
                        else const'l 0 u
  )

-- | "rangeSum" implemented in a value function.
rangeSum'v = go
  where
    go = fn @(U256 -> U256 -> U256 -> U256) "rangeSumVFn" \from step until ->
      let j = from + step
      in from + if j <=? until then call go j step until else YulEmbed 0

-- idVar :: Fn U256 U256
-- idVar = lfn "idVar" \a -> a' + a'
--   where a' = mkVar a
--   -- mkUnit a & \(a, u) ->
--   -- unVar (mkVar a) u

-- rangeSum' :: Fn (U256 :* U256 :* U256) U256
-- rangeSum' = lfn "rangeSum1" \(a :* b :* c) ->
--   mkUnit a & \(a, u) ->
--   let a' = mkVar47G a
--       b' = mkVar b
--       c' = mkVar c
--       d = a' + b'
--   in unVar d u
--   -- in ifThenElse (d <? c) (apfun rangeSum' (d :* b :* c)) (zero)

--  enumFromThenTo a b c

-- safeHead :: Fn [U256] U256
-- safeHead = lfn "safeHead" \xs -> head xs

-- safeHead :: Fn ([U256] :* ()) (One U256)
-- safeHead = lfn "safeHead" \(xs :* ()) -> yulCoerce (head xs)
  -- where go :: forall r. YulObj r => Uint256P r ⊸ Uint256P r ⊸ Uint256P r
  --       go x1 x2 = copy x1 & split & \(x1, x1') ->
  --         copy x2 & split & \(x2, x2') ->
  --         ifThenElse (x1' ?== x2') (yulCoerce x1) (yulCoerce x2)
  --         -- (copy x2 & split & \(x2, x2') -> go x2 x2')

object = mkYulObject "Basic" ctor
         [ externalFn foo1
         , externalFn foo2
           -- staticFn   foo3 -- FIXME this should not be possible with permission tag
         , staticFn rangeSum'l
         , staticFn rangeSum'v
         -- , externalFn rangeSumVFn
         ]
         where ctor = YulId -- empty constructor
