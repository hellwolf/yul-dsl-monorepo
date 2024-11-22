module ObjectDispatcherTests where

foo1 = fn'l "foo1"
  (curry'l @(U256 -> BOOL)
    \x -> dup2'l x &
    \(x1, x2) -> const'l true (sputAt (constAddr 0xdeadbeef) (x1 + x2))
  )

object :: YulObject
object = mkYulObject "ObjectDispatcherTests" emptyCtor
  [ externalFn foo1
  ]
