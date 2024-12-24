module SimpleNP_test where

-- base
import           Data.Functor.Identity (Identity)
import           Data.Type.Equality    (type (==))
-- hspec
import           Test.Hspec

--
import           Ethereum.ContractABI


test_tf_lift_function_examples = and
  [ fromBoolKind @((LiftFunction BOOL Identity Identity Many)
               == (Identity BOOL))
  , fromBoolKind @((LiftFunction (U8 -> BOOL) Identity Identity Many)
               == (Identity U8 -> Identity BOOL))
  , fromBoolKind @((LiftFunction (U8 -> U16 -> BOOL) Identity Identity Many)
               == (Identity U8 -> Identity U16 -> Identity BOOL))
  ]

test_tf_uncurry_examples = and
  [ fromBoolKind @((UncurryNP (BOOL)) == (NP '[] -> BOOL))
  , fromBoolKind @((UncurryNP (U8 -> BOOL)) == (NP '[U8] -> BOOL))
  , fromBoolKind @((UncurryNP (U8 -> U16 -> BOOL)) == (NP '[U8, U16] -> BOOL))
  , fromBoolKind @((UncurryNP (U8 -> U16 -> U24 -> BOOL)) == (NP '[U8, U16, U24] -> BOOL))
  ]

test_tf_ncurry_examples = and
  [ fromBoolKind @((CurryNP (NP '[]) U8) == U8)
  , fromBoolKind @((CurryNP (NP '[ADDR]) BOOL) == (ADDR -> BOOL))
  , fromBoolKind @((CurryNP (NP '[ADDR, U256]) BOOL) == (ADDR -> U256 -> BOOL))
  , fromBoolKind @((CurryNP (NP '[ADDR, ADDR, U256]) BOOL) == (ADDR -> ADDR -> U256 -> BOOL))
  ]

test_tf_uncurrying_head_examples = and
  [ fromBoolKind @((CurryingNP'Head (BOOL)) == ())
  , fromBoolKind @((CurryingNP'Head (U8 -> BOOL)) == U8)
  , fromBoolKind @((CurryingNP'Head (U8 -> U16 -> BOOL)) == U8)
  ]

test_tf_uncurrying_tail_examples = and
  [ fromBoolKind @((CurryingNP'Tail (BOOL)) == BOOL)
  , fromBoolKind @((CurryingNP'Tail (U8 -> BOOL)) == BOOL)
  , fromBoolKind @((CurryingNP'Tail (U8 -> U16 -> BOOL)) == (U16 -> BOOL))
  ]

tests = describe "Data.SimpleNP" $ do
  it "LiftFunction examples" test_tf_lift_function_examples
  it "UncurryNP examples" test_tf_uncurry_examples
  it "CurryNP examples" test_tf_ncurry_examples
  it "UncurryingNP'Head" test_tf_uncurrying_head_examples
  it "UncurryingNP'Tail" test_tf_uncurrying_tail_examples
