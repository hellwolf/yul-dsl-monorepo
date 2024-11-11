module ObjectDispatcherTests where

foo1 = fn'l @(U256 -> BOOL) "foo1" \x->
  copy (coerce'l @(NP '[U256]) @U256 x) & split & \(x1, x2) ->
  case toAddr 0xdeadbeef of
    Just addr -> const'l true (addr <==@ x1 + x2)

object :: YulObject
object = mkYulObject "ObjectDispatcherTests" ctor
  [ externalFn foo1
  ]
  where ctor = YulId
