module NP_test where

-- base
import           Data.Functor.Identity (Identity)
import           Data.Type.Equality    (type (==))
-- hspec
import           Test.Hspec
--
import           Ethereum.ContractABI


test_tf_lift_function_examples = and
  [ toBool' @((LiftFunction (U8 -> BOOL) Identity Identity Many) == (Identity U8 -> Identity BOOL))
  , toBool' @((LiftFunction BOOL Identity Identity Many) == (Identity BOOL))
  ]

test_tf_ncurry_examples = and
  [ toBool' @((CurryNP (NP '[]) U8) == U8)
  , toBool' @((CurryNP (NP '[ADDR, U256]) BOOL) == (ADDR -> U256 -> BOOL))
  , toBool' @((CurryNP (NP '[ADDR, ADDR, U256]) BOOL) == (ADDR -> ADDR -> U256 -> BOOL))
  ]

test_tuple_to_np = and
  [ toBool' @(TupleNtoNP () == NP '[])
  , toBool' @(TupleNtoNP (Solo U8) == NP '[U8])
  , toBool' @(TupleNtoNP (U8, U16) == NP '[U8, U16])
  ]

tests = describe "NP" $ do
  it "LiftFunction examples" test_tf_lift_function_examples
  it "NCurry examples" test_tf_ncurry_examples
  it "TupleToNP examples" test_tuple_to_np
