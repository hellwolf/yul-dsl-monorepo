module ObjectDispatcherTests where

import qualified Control.LinearlyVersionedMonad as LVM
import           Prelude.YulDSL


foo1 = fn'l "foo1" $ yulmonad'lv @(U256 -> BOOL) \x -> LVM.do
  val <- dup2'l x & \(x1, x2) -> sputAt (constAddr 0xdeadbeef) (x1 + x2)
  pure (emb'l true val)

object :: YulObject
object = mkYulObject "ObjectDispatcherTests" emptyCtor
  [ externalFn foo1
  ]
