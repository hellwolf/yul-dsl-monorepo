module Eval_prop (tests) where

import           Test.Hspec
import           Test.QuickCheck

import           LoliYul.Core
import           LoliYul.Eval

import           TestCommon      ()


test_coerce_uint256_unitprod :: UINT256 -> Bool
test_coerce_uint256_unitprod a = toInteger a == toInteger b
  where (_, (b, ())) = evalYulDSL initEvalState (YulCoerce @UINT256 @(UINT256, ())) a

test_coerce_uint256_unitprod' :: UINT256 -> Bool
test_coerce_uint256_unitprod' a = toInteger a == toInteger b
  where (_, ((), b)) = evalYulDSL initEvalState (YulCoerce @UINT256 @((), UINT256)) a

test_num_add :: UINT256 -> UINT256 -> Bool
test_num_add a b = (a + b) == c
  where (_, c) = evalYulDSL initEvalState YulNumAdd (a, b)

tests = describe "LoliYUl.Core.Eval tests" $ do
  it "YulCoerce UINT256 (UINT256,())" $ property test_coerce_uint256_unitprod
  it "YulCoerce UINT256 ((),UINT256)" $ property test_coerce_uint256_unitprod'
  it "YulNumAdd" $ property test_num_add
