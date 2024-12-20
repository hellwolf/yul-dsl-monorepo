module LinearFn_prop where

-- hspec
import           Test.Hspec
--
import           Prelude                        ()
import           Prelude.YulDSL
--
import qualified Control.LinearlyVersionedMonad as LVM

foo0 = fn'l "foo0" $
  uncurry'lv @(() -> U256) (emb'l 42)

foo1 = fn'l "foo1" $
  uncurry'lv @(U256 -> U256) id

foo2 = fn'l "foo2" $
  uncurry'lv @(U256 -> U256 -> U256)
    \x1 x2 -> x1 + x2

foo3 = fn'l "foo3" $
  uncurry'lv @(U256 -> U256 -> U256 -> U256)
    \x1 x2 x3 -> x1 + x2 + x3

foo4 = fn'l "foo4" $
  uncurry'lv @(U256 -> U256 -> U256 -> U256 -> U256)
  \x1 x2 x3 x4 -> x1 + x2 + x3 + x4

maybe_fn = fn'l "maybe_fn" $
  uncurry'lv @(Maybe U256 -> U256)
  \x1 -> match'l x1 \case
    Just g -> \v -> g v
    Nothing -> emb'l 0

bar3 = fn'l "bar3" $ yulmonad'lp @(U256 -> U256 -> U256 -> U256)
  \x1 x2 x3 -> LVM.do
  x1' <- impure x1
  x2' <- impure x2
  x3' <- impure x3
  pure (x1' + x2' + x3')

fooSPut = fn'l "fooSPut" $ yulmonad'lv @(ADDR -> U256 -> ())
  \addr val -> LVM.do
  sput_ addr val

call0 = fn'l "call0" $
  uncurry'lv @(() -> U256)
    \u -> call'l foo0 u

call1 = fn'l "call1" $
  uncurry'lv @(U256 -> U256)
    \x -> call'l foo1 x

call2 = fn'l "call2" $
  uncurry'lv @(U256 -> U256 -> U256)
    \x1 x2 -> call'l foo2 x1 x2

call3 = fn'l "call3" $
  uncurry'lv @(U256 -> U256 -> U256 -> U256)
    \x1 x2 x3 -> call'l foo3 x1 x2 x3

call4 = fn'l "call4" $
  uncurry'lv @(U256 -> U256 -> U256 -> U256 -> U256)
    \x1 x2 x3 x4 -> call'l foo4 x1 x2 x3 x4

callSPut = fn'l "callSPut" $
  uncurry'lv @(ADDR -> U256 -> ())
    \addr var -> call'l fooSPut addr var

tests = describe "Ethereum.ContractsABI.YulDSL.Linear" $ do
  describe "fn'l: linear function builder" $ do
    it "simple fn definitions" True
