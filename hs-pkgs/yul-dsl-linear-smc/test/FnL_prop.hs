module FnL_prop where

-- hspec
import           Test.Hspec
--
import           Prelude                  ()
import           Prelude.YulDSL.LinearSMC

foo1 = fn'l "foo1"
  ( curry'l @(Maybe U256 -> Maybe U256)
    \x -> x
  )

foo2 = fn'l "foo2"
  ( curry'l @(Maybe U256 -> Maybe U256 -> Maybe U256)
    \x1 x2 -> x1 + x2
  )

foo3 = fn'l "foo3"
  ( curry'l @(Maybe U256 -> Maybe U256 -> Maybe U256 -> Maybe U256)
    \x1 x2 x3 -> x1 + x2 + x3
  )

foo4 = fn'l "foo4"
  ( curry'l @(Maybe U256 -> Maybe U256 -> Maybe U256 -> Maybe U256 -> Maybe U256)
    \x1 x2 x3 x4 -> x1 + x2 + x3 + x4
  )

call1 = fn'l "call1"
  ( curry'l @(Maybe U256 -> Maybe U256)
    \x -> call'l foo1 x
  )

call2 = fn'l "call2"
  ( curry'l @(Maybe U256 -> Maybe U256 -> Maybe U256)
    \x1 x2 -> call'l foo2 x1 x2
  )

call3 = fn'l "call3"
  ( curry'l @(Maybe U256 -> Maybe U256 -> Maybe U256 -> Maybe U256)
    \x1 x2 x3 -> call'l foo3 x1 x2 x3
  )

call4 = fn'l "call4"
  ( curry'l @(Maybe U256 -> Maybe U256 -> Maybe U256 -> Maybe U256 -> Maybe U256)
    \x1 x2 x3 x4 -> call'l foo4 x1 x2 x3 x4
  )

tests = describe "YulDSL.LinearSMC" $ do
  describe "fn'l: linear function builder" $ do
    it "simple fn definitions" True
