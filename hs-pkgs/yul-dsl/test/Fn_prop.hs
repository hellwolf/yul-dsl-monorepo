module Fn_prop where

import           Test.Hspec
-- import           Test.QuickCheck

import           Ethereum.ContractABI
--
import           YulDSL.Core


foo0 :: Fn (Maybe U8)
foo0 = fn @(Maybe U8) "id" (YulEmbed (Just 42))

-- foo1 :: Fn (Maybe U8 -> Maybe U8)
foo1 = fn @(Maybe U8 -> Maybe U8) "id"
       (\a -> a + a)

-- {-# ANN foo2 "HLint: ignore Avoid lambda" #-}
foo2 = fn @(Maybe U8 -> Maybe U8 -> Maybe U8)
       "id"
       (\a b -> a + b)

foo3 = fn @(Maybe U8 -> Maybe U8 -> Maybe U8 -> Maybe U8) "id"
       (\a b c -> a + b + c)

foo4 = fn @(Maybe U8 -> Maybe U8 -> Maybe U8 -> Maybe U8 -> Maybe U8) "id"
       (\a b c d -> a + b + c + d)

test_simple_fn = True

tests = describe "YulDSL.Core.Fn" $ do
  describe "fn: pure function builder" $ do
    it "simple fn definitions" test_simple_fn
