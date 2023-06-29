module Eval_prop (tests) where

import           Test.Hspec
import           Test.QuickCheck


import           LoliYul.Core
import           LoliYul.Eval

import           TestCommon      ()

test_num_add :: UINT256 -> UINT256 -> Bool
test_num_add a b = (a + b) == c
  where (_, c) = evalYulDSL initEvalState YulNumAdd (a, b)

tests = describe "LoliYUl.Core.Eval tests" $ do
  it "YulNumAdd" $ property test_num_add
