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

test_tuple_to_np = and
  [ toBool' @(TupleNtoNP () == NP '[])
  , toBool' @(TupleNtoNP (Solo U8) == NP '[U8])
  , toBool' @(TupleNtoNP (U8, U16) == NP '[U8, U16])
  ]

test_from_tuple_to_np = and
  [ fromTupleNPtoNP () == Nil
  , fromTupleNPtoNP (MkSolo "hi") == "hi" :* Nil
  , fromTupleNPtoNP (1, 2) == 1 :* 2 :* Nil
  , fromTupleNPtoNP (1, 2) /= 2 :* 1 :* Nil
  , fromTupleNPtoNP (1, 2, ("hello" :* Nil)) == 1 :* 2 :* ("hello" :* Nil) :* Nil
  , fromTupleNPtoNP (1, 2, 3) /= 3 :* 2 :* 1 :* Nil
  ]

test_from_np_to_tuple = and
  [ fromNPtoTupleN Nil == ()
  , fromNPtoTupleN ("nihao" :* Nil) == "nihao"
  , fromNPtoTupleN (1 :* 2 :* Nil) == (1, 2)
  , fromNPtoTupleN (1 :* 2 :* Nil) /= (2, 1)
  , fromNPtoTupleN (1 :* 2 :* ("konijiwa" :* Nil) :* Nil) == (1, 2, "konijiwa" :* Nil)
  , fromNPtoTupleN (1 :* 2 :* ("konijiwa" :* Nil) :* Nil) /= (1, 2, "nani" :* Nil)
  ]

tests = describe "TupleN tests" $ do
  it "TupleToNP examples" test_tuple_to_np
  it "fromTupleNPToNP examples" test_from_tuple_to_np
  it "fromNPtoTupleN examples" test_from_np_to_tuple
