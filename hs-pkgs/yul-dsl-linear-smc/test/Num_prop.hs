module Num_prop where

import           Test.Hspec
--
import           Prelude        ()
import           Prelude.YulDSL


num_add_maybe = fn'l "add2"
  ( curry'l @(Maybe U256 -> Maybe U256 -> Maybe U256)
    \x1 x2 -> x1 + x2
  )

num_add_just = fn'l "add2"
  ( curry'l @(U256 -> U256 -> U256)
    \x1 x2 -> x1 + x2
  )

tests = describe "Data.Num.YulDSL.Linear" $ do
  it "Num hierarchy classes" True
