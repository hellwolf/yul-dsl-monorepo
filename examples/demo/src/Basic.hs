{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Basic where

------------------------------------------------------------------------------------------------------------------------
-- Trivial Diagrams
------------------------------------------------------------------------------------------------------------------------

simple_id = MkFn "simple_id" $ YulId @U256

simple_coerce = MkFn "simple_coerce" $ YulCoerce @U256 @U256

const_42 = MkFn "const_42" $ decode (yulConst (to_intx 42)) :: Fn () U256

nop :: Fn () ()
nop = lfn "nop" \(u) -> u

disFn :: ABITypeable a => Fn a ()
disFn = MkFn "disFn" YulDis

mkConst :: ABITypeable a => a -> Fn () a
mkConst a = lfn "mkConst" \u -> yulConst a u

-- | A function that takes one uint and store its value doubled at a fixed storage location.
foo1 :: Fn (U256 -> U256)
foo1 = lfn "foo1" \x ->
  dup2P x & \(x, x') -> x + x'

-- | A function takes two uints and store their sum at a fixed storage location then returns true.
--
--   Note: you can create any number of "unit" signals by adding '()' to the input list.
foo2 :: Fn (U256 -> U256 -> U256)
foo2 = lfn "foo2" \(x1 :* x2) ->
  dup2P x2 & \(x2, x2') ->
  x1 + (x2 + x2')

-- | A function takes two uints and store their sum at a fixed storage location then returns it.
foo3 :: Fn (U256 :* U256 :* ()) (BOOL, U256)
foo3 = lfn "foo3" \(x1 :* x2 :* u) ->
  dup2P (x1 + x2) & \(r, r') ->
  ignore (to_addr' 0xdeadbeef <==@ r) (merge (yulConst true u, r'))

-- | Sum a range @[i..t]@ of numbers separated by a step number @s@ as a linear function.
rangeSumLFn :: Fn (U256 :* U256 :* U256) U256
rangeSumLFn = lfn "rangeSumLFn" \(from :* step :* until) ->
  mkUnit from & \(from, u) -> dup2P from & \(from, from') ->
  dup2P step & \(step, step') ->
  dup2P until & \(until, until') ->
  dup2P (from + step) & \ (j, j') ->
  from' + if j <=? until then ap'lfn rangeSumLFn (j' :* step' :* until') else yulConst 0 u

-- | "rangeSum" implemented in a value function.
rangeSumVFn :: Fn (U256 :* U256 :* U256) U256
rangeSumVFn = vfn "rangeSumVFn" \(from :* step :* until) ->
    ap'vfn go (from :* step :* until)
  where
    go :: Fn (U256 :* U256 :* U256) U256
    go = vfn "rangeSumVFn_go" \(from :* step :* until) ->
      let j = from + step
      in from + if j <=? until then ap'vfn go (j :* step :* until) else YulEmbed 0

-- idVar :: Fn U256 U256
-- idVar = lfn "idVar" \a -> a' + a'
--   where a' = mkVar a
--   -- mkUnit a & \(a, u) ->
--   -- unVar (mkVar a) u

-- rangeSum' :: Fn (U256 :* U256 :* U256) U256
-- rangeSum' = lfn "rangeSum1" \(a :* b :* c) ->
--   mkUnit a & \(a, u) ->
--   let a' = mkVar a
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
         , staticFn rangeSumLFn
         , staticFn rangeSumVFn
         -- , externalFn rangeSumVFn
         ]
         where ctor = YulId -- empty constructor
