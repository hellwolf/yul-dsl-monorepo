module LinearFn_prop where

-- hspec
import Test.Hspec
--
import Prelude                        ()
import Prelude.YulDSL
--
import Control.LinearlyVersionedMonad qualified as LVM

foo0 = lfn "foo0" $
  uncurry'lvv @(() -> U256) (emb'l 42)

foo1 = lfn "foo1" $
  uncurry'lvv @(U256 -> U256) id

foo2 = lfn "foo2" $
  uncurry'lvv @(U256 -> U256 -> U256)
    \x1 x2 -> x1 + x2

foo3 = lfn "foo3" $
  uncurry'lvv @(U256 -> U256 -> U256 -> U256)
    \x1 x2 x3 -> x1 + x2 + x3

foo4 = lfn "foo4" $
  uncurry'lvv @(U256 -> U256 -> U256 -> U256 -> U256)
  \x1 x2 x3 x4 -> x1 + x2 + x3 + x4

maybe_fn = lfn "maybe_fn" $
  uncurry'lvv @(Maybe U256 -> U256)
  \x1 -> match'l x1 \case
    Just g -> \v -> g v
    Nothing -> emb'l 0

bar3 = lfn "bar3" $ yulmonad'p @(U256 -> U256 -> U256 -> U256)
  \x1 x2 x3 -> LVM.do
  x1' <- impure x1
  x2' <- impure x2
  x3' <- impure x3
  pure (x1' + x2' + x3')

fooSPut = lfn "fooSPut" $ yulmonad'v @(B32 -> U256 -> ())
  \addr val -> LVM.do
  sput_ addr val

call0 = lfn "call0" $
  uncurry'lvv @(() -> U256)
    \u -> callLfn'l foo0 u

call1 = lfn "call1" $
  uncurry'lvv @(U256 -> U256)
    \x -> callLfn'l foo1 x

call2 = lfn "call2" $
  uncurry'lvv @(U256 -> U256 -> U256)
    \x1 x2 -> callLfn'l foo2 x1 x2

call3 = lfn "call3" $
  uncurry'lvv @(U256 -> U256 -> U256 -> U256)
    \x1 x2 x3 -> callLfn'l foo3 x1 x2 x3

call4 = lfn "call4" $
  uncurry'lvv @(U256 -> U256 -> U256 -> U256 -> U256)
    \x1 x2 x3 x4 -> callLfn'l foo4 x1 x2 x3 x4

callSPut = lfn "callSPut" $
  uncurry'lvv @(B32 -> U256 -> ())
    \addr var -> callLfn'l fooSPut addr var

tests = describe "Ethereum.ContractsABI.YulDSL.Linear" $ do
  describe "lfn: linear function builder" $ do
    it "simple fn definitions" True
