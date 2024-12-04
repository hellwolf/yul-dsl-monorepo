module SimpleNP_test where

-- base
import           Data.Functor.Identity (Identity)
import           Data.Type.Equality    (type (==))
-- hspec
import           Test.Hspec

--
import           Ethereum.ContractABI


test_tf_lift_function_examples = and
  [ toBool' @((LiftFunction BOOL Identity Identity Many)
               == (Identity BOOL))
  , toBool' @((LiftFunction (U8 -> BOOL) Identity Identity Many)
               == (Identity U8 -> Identity BOOL))
  , toBool' @((LiftFunction (U8 -> U16 -> BOOL) Identity Identity Many)
               == (Identity U8 -> Identity U16 -> Identity BOOL))
  ]

test_tf_uncurry_examples = and
  [ toBool' @((UncurryNP (BOOL)) == (NP '[] -> BOOL))
  , toBool' @((UncurryNP (U8 -> BOOL)) == (NP '[U8] -> BOOL))
  , toBool' @((UncurryNP (U8 -> U16 -> BOOL)) == (NP '[U8, U16] -> BOOL))
  , toBool' @((UncurryNP (U8 -> U16 -> U24 -> BOOL)) == (NP '[U8, U16, U24] -> BOOL))
  ]

test_tf_ncurry_examples = and
  [ toBool' @((CurryNP (NP '[]) U8) == U8)
  , toBool' @((CurryNP (NP '[ADDR]) BOOL) == (ADDR -> BOOL))
  , toBool' @((CurryNP (NP '[ADDR, U256]) BOOL) == (ADDR -> U256 -> BOOL))
  , toBool' @((CurryNP (NP '[ADDR, ADDR, U256]) BOOL) == (ADDR -> ADDR -> U256 -> BOOL))
  ]

test_tf_uncurrying_head_examples = and
  [ toBool' @((CurryingNP'Head (BOOL)) == ())
  , toBool' @((CurryingNP'Head (U8 -> BOOL)) == U8)
  , toBool' @((CurryingNP'Head (U8 -> U16 -> BOOL)) == U8)
  ]

test_tf_uncurrying_tail_examples = and
  [ toBool' @((CurryingNP'Tail (BOOL)) == BOOL)
  , toBool' @((CurryingNP'Tail (U8 -> BOOL)) == BOOL)
  , toBool' @((CurryingNP'Tail (U8 -> U16 -> BOOL)) == (U16 -> BOOL))
  ]

tests = describe "Data.SimpleNP" $ do
  it "LiftFunction examples" test_tf_lift_function_examples
  it "UncurryNP examples" test_tf_uncurry_examples
  it "CurryNP examples" test_tf_ncurry_examples
  it "UncurryingNP'Head" test_tf_uncurrying_head_examples
  it "UncurryingNP'Tail" test_tf_uncurrying_tail_examples
