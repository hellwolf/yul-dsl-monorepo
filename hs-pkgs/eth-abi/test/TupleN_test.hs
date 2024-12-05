{-# OPTIONS_GHC -Wno-type-defaults #-}
module TupleN_test where

-- base
import           Data.Type.Equality   (type (==))
-- hspec
import           Test.Hspec
--
import           Data.TupleN          (Solo (MkSolo))
import           Ethereum.ContractABI


default (U256)

test_tf_tuple_to_np = and
  [ toBool' @(TupleNtoNP () == NP '[])
  , toBool' @(TupleNtoNP (Solo U8) == NP '[U8])
  , toBool' @(TupleNtoNP (U8, U16) == NP '[U8, U16])
  ]

test_tf_np_to_tuple = and
  [ toBool' @(NPtoTupleN (NP '[]) == ())
  , toBool' @(NPtoTupleN (NP '[U8]) == Solo U8)
  , toBool' @(NPtoTupleN (NP '[U8, U16]) == (U8, U16))
  , toBool' @(NPtoTupleN (NP '[U8, U16, U32]) == (U8, U16, U32))
  ]

test_from_tuple_to_np = and
  [ fromTupleNtoNP () == Nil
  , fromTupleNtoNP (MkSolo "hi") == "hi" :* Nil
  , fromTupleNtoNP (1, 2) == 1 :* 2 :* Nil
  , fromTupleNtoNP (1, 2) /= 2 :* 1 :* Nil
  , fromTupleNtoNP (1, 2, ("hello" :* Nil)) == 1 :* 2 :* ("hello" :* Nil) :* Nil
  , fromTupleNtoNP (1, 2, 3) /= 3 :* 2 :* 1 :* Nil
  ]

test_from_np_to_tuple = and
  [ fromNPtoTupleN Nil == ()
  , fromNPtoTupleN ("nihao" :* Nil) == (MkSolo "nihao")
  , fromNPtoTupleN (1 :* 2 :* Nil) == (1, 2)
  , fromNPtoTupleN (1 :* 2 :* Nil) /= (2, 1)
  , fromNPtoTupleN (1 :* 2 :* ("konijiwa" :* Nil) :* Nil) == (1, 2, "konijiwa" :* Nil)
  , fromNPtoTupleN (1 :* 2 :* ("konijiwa" :* Nil) :* Nil) /= (1, 2, "nani" :* Nil)
  ]

tests = describe "TupleN tests" $ do
  it "TupleNtoNP examples" test_tf_tuple_to_np
  it "NPtoTupleN examples" test_tf_np_to_tuple
  it "fromTupleNtoNP examples" test_from_tuple_to_np
  it "fromNPtoTupleN examples" test_from_np_to_tuple
