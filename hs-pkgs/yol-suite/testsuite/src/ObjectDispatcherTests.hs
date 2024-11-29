module ObjectDispatcherTests where

foo1 = fn'l "foo1"
  -- FIXME use curry'pl
  (uncurry'l @(U256 -> BOOL)
    \x -> runLT $
    dup2'l x
    & \(x1, x2) -> sputAt (constAddr 0xdeadbeef) (x1 + x2)
    |> fin'emb true
  )

object :: YulObject
object = mkYulObject "ObjectDispatcherTests" emptyCtor
  [ externalFn foo1
  ]
