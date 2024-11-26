module ObjectDispatcherTests where

foo1 = fn'l "foo1"
  -- FIXME use curry'pl
  (uncurry'l @(U256 -> BOOL)
    \x -> dup'l x
    & \(x1, x2) -> emb'l true (sputAt (constAddr 0xdeadbeef) (x1 + x2))
  )

object :: YulObject
object = mkYulObject "ObjectDispatcherTests" emptyCtor
  [ externalFn foo1
  ]
