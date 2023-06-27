{-# OPTIONS_GHC -Wno-orphans #-}

module Eval_prop (tests) where

import           Data.Functor    ((<&>))

import           Test.Hspec
import           Test.QuickCheck


import           LoliYul.Core

instance Arbitrary UINT256 where
  arbitrary = arbitrary <&> to_uint256 -- FIXME use chooseBoundedIntegral

test_num_add :: UINT256 -> UINT256 -> Bool
test_num_add a b = (a + b) == c
  where (_, c) = evalYulDSL initEvalState YulNumAdd (a, b)

tests = describe "LoliYUl.Core.Eval tests" $ do
  it "YulNumAdd" $ property test_num_add
