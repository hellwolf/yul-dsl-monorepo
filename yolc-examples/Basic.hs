{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Basic where

------------------------------------------------------------------------------------------------------------------------
-- Trivial Diagrams
------------------------------------------------------------------------------------------------------------------------

simple_id = YulId @UINT256
simple_coerce = YulCoerce @UINT256 @UINT256
const_42 = decode (yulConst (to_intx 42)) :: YulCat () UINT256

------------------------------------------------------------------------------------------------------------------------
-- Yul Internal Functions
------------------------------------------------------------------------------------------------------------------------

nop :: Fn () ()
nop = externalFn "nop" $ lfn \(u) -> u

disFn :: YulPortReducible a => Fn a ()
disFn = libraryFn "disFn" YulDis

mkConst :: YulPortReducible a => a -> Fn () a
mkConst a = libraryFn "mkConst" $ lfn \u -> yulConst a u

-- | A function that takes one uint and store its value doubled at a fixed storage location.
foo1 :: Fn UINT256 BOOL
foo1 = externalFn "foo" $ lfn \x ->
  copy x & split & \(x1, x2) ->
  yulConst true (to_addr' 0xdeadbeef <==@ x1 + x2)

-- | A function takes two uints and store their sum at a fixed storage location then returns true.
--
--   Note: you can create any number of "unit" signals by adding '()' to the input list.
foo2 :: Fn (UINT256 :* UINT256 :* ()) BOOL
foo2 = externalFn "foo2" $ lfn \(x1 :* x2 :* u) ->
  (to_addr' 0xdeadbeef <==@ x1 + x2) &
  ignore u & yulConst true

-- | A function takes two uints and store their sum at a fixed storage location then returns it.
foo3 :: Fn (UINT256 :* UINT256 :* ()) (BOOL, UINT256)
foo3 = externalFn "foo3" $ lfn \(x1 :* x2 :* u) ->
  dup2P (x1 + x2) & \(r, r') ->
  ignore (to_addr' 0xdeadbeef <==@ r) (merge (yulConst true u, r'))

-- | Sum a range @[i..t]@ of numbers separated by a step number @s@ as a linear function.
rangeSumLFn :: Fn (UINT256 :* UINT256 :* UINT256) UINT256
rangeSumLFn = libraryFn "rangeSumLFn" $ lfn \(i :* s :* t) ->
  dup2P i & \(i, i') ->
  dup2P s & \(s, s') ->
  dup2P t & \(t, t') ->
  dup2P (i + s) & \ (j, j') ->
  mkUnit i' & \(i', u) ->
  i' + if j <=? t then ap'lfn rangeSumLFn (j' :* s' :* t') else yulConst 0 u

-- | "rangeSum" implemented in a value function.
rangeSumVFn :: Fn (UINT256 :* UINT256 :* UINT256) UINT256
rangeSumVFn = libraryFn "rangeSumVFn" $ vfn \(i :* s :* t) ->
  ap'vfn go (YulEmbed 0 :* i :* s :* t)
  where
    go :: Fn (UINT256 :* UINT256 :* UINT256 :* UINT256) UINT256
    go = libraryFn "rangeSumVFn_go" $ vfn \(c :* i :* s :* t) ->
          if (i + s) <=? t then ap'vfn go (c + i :* i + s :* s :* t) else c

-- idVar :: Fn UINT256 UINT256
-- idVar = lfn "idVar" \a -> a' + a'
--   where a' = mkVar a
--   -- mkUnit a & \(a, u) ->
--   -- unVar (mkVar a) u

-- rangeSum' :: Fn (UINT256 :* UINT256 :* UINT256) UINT256
-- rangeSum' = lfn "rangeSum1" \(a :* b :* c) ->
--   mkUnit a & \(a, u) ->
--   let a' = mkVar a
--       b' = mkVar b
--       c' = mkVar c
--       d = a' + b'
--   in unVar d u
--   -- in ifThenElse (d <? c) (apfun rangeSum' (d :* b :* c)) (zero)

--  enumFromThenTo a b c

-- safeHead :: Fn [UINT256] UINT256
-- safeHead = lfn "safeHead" \xs -> head xs

-- safeHead :: Fn ([UINT256] :* ()) (One UINT256)
-- safeHead = lfn "safeHead" \(xs :* ()) -> yulCoerce (head xs)
  -- where go :: forall r. YulObj r => Uint256P r ⊸ Uint256P r ⊸ Uint256P r
  --       go x1 x2 = copy x1 & split & \(x1, x1') ->
  --         copy x2 & split & \(x2, x2') ->
  --         ifThenElse (x1' ?== x2') (yulCoerce x1) (yulCoerce x2)
  --         -- (copy x2 & split & \(x2, x2') -> go x2 x2')

object = mkYulObject "Basic" ctor
         [ MkAnyFn foo1
         , MkAnyFn foo2
         , MkAnyFn foo3
         , MkAnyFn rangeSumLFn
         , MkAnyFn rangeSumVFn
         ]
         where ctor = YulId -- empty constructor
