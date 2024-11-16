module Fn_prop where

import           Test.Hspec
-- import           Test.QuickCheck

import           Ethereum.ContractABI
--
import           YulDSL.Core


foo0 :: Fn (U8)
foo0 = fn @(U8) "id" (YulEmbed 42)

foo1 :: Fn (U8 -> U8)
foo1 = fn @(U8 -> U8) "id"
       (\a -> a + a)

foo2 = fn @(U8 -> U8 -> U8)
       "id"
       (\a b -> a + b)

foo3 = fn @(U8 -> U8 -> U8 -> U8) "id"
       (\a b c -> a + b + c)

foo4 = fn @(U8 -> U8 -> U8 -> U8 -> U8) "id"
       (\a b c d -> a + b + c + d)

call0 = fn @(U8) "id"
  (call foo0)

call1 = fn @(U8 -> U8) "id"
  (\a -> call foo1 a)

call2 = fn @(U8 -> U8) "id"
  (\a -> call foo2 a a)

call3 = fn @(U8 -> U8) "id"
  (\a -> call foo3 a a a)

call4 = fn @(U8 -> U8) "id"
  (\a -> call foo4 a a a a)

test_simple_fn = True

tests = describe "YulDSL.Core.Fn" $ do
  describe "fn: pure function builder" $ do
    it "simple fn definitions" test_simple_fn
