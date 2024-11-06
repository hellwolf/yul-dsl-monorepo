{-# LANGUAGE TypeFamilies #-}
module NP_test where

import           Data.Functor.Identity (Identity)
--
import           Test.Hspec
--
import           Ethereum.ContractABI


type family TypeEq a b where
  TypeEq a a = True
  TypeEq _ _ = False

test_tf_lift_function_examples = and
  [ toBool' @(TypeEq (LiftFunction (U8 -> BOOL) Identity Many) (Identity U8 -> Identity BOOL))
  , toBool' @(TypeEq (LiftFunction BOOL Identity Many) (Identity BOOL))
  ]

test_tf_ncurry_examples = and
  [ toBool' @(TypeEq (CurryNP (NP '[]) U8) U8)
  , toBool' @(TypeEq (CurryNP (NP '[ADDR, U256]) BOOL) (ADDR -> U256 -> BOOL))
  , toBool' @(TypeEq (CurryNP (NP '[ADDR, ADDR, U256]) BOOL) (ADDR -> ADDR -> U256 -> BOOL))
  ]

tests = describe "NP" $ do
  it "LiftFunction examples" $ test_tf_lift_function_examples
  it "NCurry examples" $ test_tf_ncurry_examples
