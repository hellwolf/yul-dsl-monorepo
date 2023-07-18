{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Basic where

import           LoliYul.Core

------------------------------------------------------------------------------------------------------------------------
-- Trivial Diagrams
------------------------------------------------------------------------------------------------------------------------

simple_id = YulId @UINT256
simple_coerce = YulCoerce @UINT256 @INT256
const_42 = decode (yulConst (to_intx 42)) :: YulDSL () UINT256

------------------------------------------------------------------------------------------------------------------------
-- Yul Internal Functions
------------------------------------------------------------------------------------------------------------------------

nop :: Fn () ()
nop = defun "noop" \(u) -> u

-- FIXME defun cannot express this:
-- ignoreFn :: YulPortReducible a => Fn a ()
-- ignoreFn = defun "ignoreFn" \a -> yulConst () a

mkConst :: YulPortReducible a => a -> Fn () a
mkConst a = defun "mkConst" \u -> yulConst a u

-- | A function that takes a UInt input and store its value doubled at a fixed storage location.
foo :: Fn UINT256 BOOL
foo = defun "foo" \x ->
  (copy x & split & \(x1, x2) ->
      to_addr' 0xdeadbeef <=@ x1 + x2
  ) & yulConst true

foo2 :: Fn (UINT256 :> UINT256 :> ()) BOOL
foo2 = defun "foo2" \(x1 :> x2 :> u) ->
  (to_addr' 0xdeadbeef <=@ x1 + x2) &
  ignore u & yulConst true

foo3 :: Fn (UINT256 :> UINT256 :> ()) (UINT256, BOOL)
foo3 = defun "foo3" \(x1 :> x2 :> u) ->
  (to_addr' 0xdeadbeef <=@ x1 + x2) &
  ignore u & yulConst (24, true)

rangeSum :: Fn (UINT256 :> UINT256 :> UINT256) UINT256
rangeSum = defun "rangeSum" \(a :> b :> c) ->
  copy a & split & \(a, a') ->
  copy b & split & \(b, b') ->
  copy c & split & \(c, c') ->
  copy (a + b) & split & \ (d, d') ->
  mkUnit a' & \(a', u) ->
  a' + ifThenElse (d <? c) (apfun rangeSum (d' :> b' :> c')) (yulConst 0 u)

--  enumFromThenTo a b c

-- safeHead :: Fn [UINT256] UINT256
-- safeHead = defun "safeHead" \xs -> head xs

-- safeHead :: Fn ([UINT256] :> ()) (One UINT256)
-- safeHead = defun "safeHead" \(xs :> ()) -> yulCoerce (head xs)
  -- where go :: forall r. YulObj r => Uint256P r ⊸ Uint256P r ⊸ Uint256P r
  --       go x1 x2 = copy x1 & split & \(x1, x1') ->
  --         copy x2 & split & \(x2, x2') ->
  --         ifThenElse (x1' ?== x2') (yulCoerce x1) (yulCoerce x2)
  --         -- (copy x2 & split & \(x2, x2') -> go x2 x2')
