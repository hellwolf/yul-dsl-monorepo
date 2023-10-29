module ObjectDispatcherTests where


foo1 :: Fn UINT256 BOOL
foo1 = lfn "foo1" \x ->
  copy x & split & \(x1, x2) ->
  yulConst true (to_addr' 0xdeadbeef <==@ x1 + x2)

object = mkYulObject "ObjectDispatcherTests" ctor
  [ externalFn foo1
  ]
  where ctor = YulId
