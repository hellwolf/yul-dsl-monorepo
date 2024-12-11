module Fn_prop where

-- base
import           Data.Functor         ((<&>))
-- hspec, quickcheck
import           Test.Hspec
import           Test.QuickCheck
--
import           Ethereum.ContractABI
--
import           YulDSL.Core
import           YulDSL.Eval
--
import           TestCommon           ()


dis_any :: forall a. YulO1 a => PureFn (a -> ())
dis_any = fn "" \_ -> emb'p ()

uncurry_fn0 :: PureFn (U256)
uncurry_fn0 = fn @(U256) "" (emb'p 42)

uncurry_fn1 :: PureFn (U256 -> U256)
uncurry_fn1 = fn ""
  \a -> a + a

uncurry_fn2 = fn @(U256 -> U256 -> U256) ""
  \a b -> a + b

uncurry_fn3 = fn @(U256 -> U256 -> U256 -> U256) ""
  \a b c -> a + b + c

uncurry_fn4 = fn @(U256 -> U256 -> U256 -> U256 -> U256) ""
  \a b c d -> a + b + c + d

call_fn0 = fn @(U256) ""
  do call'p uncurry_fn0

call_fn1 = fn @(U256 -> U256) ""
  \a -> call'p uncurry_fn1 a

call_fn2 = fn @(U256 -> U256) ""
  \a -> call'p uncurry_fn2 a a

call_fn3 = fn @(U256 -> U256) ""
  \a -> call'p uncurry_fn3 a a a

call_fn4 = fn @(U256 -> U256) ""
  \a -> call'p uncurry_fn4 a a a a

maybe_num_fn2 = fn @(Maybe U8 -> Maybe U8 -> U8) ""
  \a b -> match (a + b) \case
    Just x -> x
    Nothing -> 0

test_simple_fn :: Gen Bool
test_simple_fn = chooseInteger (0, toInteger (maxBound @U32)) <&>
  (\x -> and
    [ evalFn call_fn1 (x :* Nil) == x + x
    , evalFn call_fn2 (x :* Nil) == x + x
    , evalFn call_fn3 (x :* Nil) == x + x + x
    , evalFn call_fn4 (x :* Nil) == x + x + x + x
    ]
  ) . fromInteger

test_maybe_fn :: Bool
test_maybe_fn = and
  [ evalFn maybe_num_fn2 (Just 42 :* Just 69 :* Nil) == 111
  , evalFn maybe_num_fn2 (Just 255 :* Just 255 :* Nil) == 0
  ]

tests = describe "YulDSL.Core.Fn" $ do
  it "simple fn" $ property test_simple_fn
  it "pattern matching with Maybe" $ property test_maybe_fn
