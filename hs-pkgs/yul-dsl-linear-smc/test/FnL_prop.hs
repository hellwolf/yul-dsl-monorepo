module FnL_prop where

-- hspec
import           Test.Hspec
--
import           Prelude        ()
import           Prelude.YulDSL

foo0 = fn'l "foo0"
  (curry'l @(() -> U256) (const'l 42)
  )

foo1 = fn'l "foo1"
  (curry'l @(U256 -> U256) id
  )

foo2 = fn'l "foo2"
  (curry'l @(U256 -> U256 -> U256)
    \x1 x2 -> x1 + x2
  )

foo3 = fn'l "foo3"
  (curry'l @(U256 -> U256 -> U256 -> U256)
    \x1 x2 x3 -> x1 + x2 + x3
  )

foo4 = fn'l "foo4"
  (curry'l @(U256 -> U256 -> U256 -> U256 -> U256)
    \x1 x2 x3 x4 -> x1 + x2 + x3 + x4
  )

fooSPut = fn'l "fooSPut"
  (curry'l @(ADDR -> U256 -> ())
   \addr val -> dis'l (sput addr val))

call0 = fn'l "call0"
  (curry'l @(() -> U256)
    \u -> call'l foo0 u
  )

call1 = fn'l "call1"
  (curry'l @(U256 -> U256)
    \x -> call'l foo1 x
  )

call2 = fn'l "call2"
  (curry'l @(U256 -> U256 -> U256)
    \x1 x2 -> call'l foo2 x1 x2
  )

call3 = fn'l "call3"
  (curry'l @(U256 -> U256 -> U256 -> U256)
    \x1 x2 x3 -> call'l foo3 x1 x2 x3
  )

call4 = fn'l "call4"
  (curry'l @(U256 -> U256 -> U256 -> U256 -> U256)
    \x1 x2 x3 x4 -> call'l foo4 x1 x2 x3 x4
  )

callSPut = fn'l "callSPut"
  (curry'l @(ADDR -> U256 -> ())
    \addr var -> call'l fooSPut addr var)

tests = describe "Ethereum.ContractsABI.YulDSL.Linear" $ do
  describe "fn'l: linear function builder" $ do
    it "simple fn definitions" True
