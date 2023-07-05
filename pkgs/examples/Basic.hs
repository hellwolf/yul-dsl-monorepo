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

-- | A function that takes a UInt input and store its value doubled at a fixed storage location.
foo :: Fn (One UINT256) (One BOOL)
foo = defun "foo" \(x :> ()) ->
  (copy x & split & \(x1, x2) ->
      to_addr' 0xdeadbeef <=@ x1 + x2
  ) &
  yulConst (true :> ())

foo2 :: Fn (UINT256 :> UINT256 :> ()) (One BOOL)
foo2 = defun "foo" \(x1 :> x2 :> ()) ->
  (to_addr' 0xdeadbeef <=@ x1 + x2) &
  yulConst (true :> ())
