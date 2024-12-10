module Fn_prop where

import           Test.Hspec
-- import           Test.QuickCheck

import           Ethereum.ContractABI
--
import           YulDSL.Core


disFn :: YulO1 a => PureFn (a -> ())
disFn = MkFn (MkFnCat "disFn" YulDis)

simple_id = MkFnCat @MkPure @U256 @U256 "simple_id" YulId

simple_coerce = MkFnCat "simple_coerce" $ YulCoerce @MkPure @U256 @U256

foo0 :: PureFn (U8)
foo0 = fn @(U8) "" (emb'p 42)

foo1 :: PureFn (U8 -> U8)
foo1 = fn @(U8 -> U8) ""
       \a -> a + a

foo2 = fn @(U8 -> U8 -> U8) ""
       \a b -> a + b

foo3 = fn @(U8 -> U8 -> U8 -> U8) ""
       \a b c -> a + b + c

foo4 = fn @(U8 -> U8 -> U8 -> U8 -> U8) ""
       \a b c d -> a + b + c + d

bar = fn'p "" $ uncurry'p @(U256 -> U256)
  \a -> a + a

call'p0 = fn @(U8) ""
  do call'p foo0

call'p1 = fn @(U8 -> U8) ""
  \a -> call'p foo1 a

call'p2 = fn @(U8 -> U8) ""
  \a -> call'p foo2 a a

call'p3 = fn @(U8 -> U8) ""
  \a -> call'p foo3 a a a

call'p4 = fn @(U8 -> U8) ""
  \a -> call'p foo4 a a a a

test_simple_fn = True

tests = describe "YulDSL.Core.Fn" $ do
  describe "fn: pure function builder" $ do
    it "simple fn definitions" test_simple_fn
