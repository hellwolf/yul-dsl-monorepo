module Num_Tests where

import           Prelude.YulDSL

add_uint256 = fn @(U256 -> U256 -> U256) "add_uint256" $ \x y -> x + y
add_uint128 = fn @(U128 -> U128 -> U128) "add_uint128" $ \x y -> x + y
add_uint32  = fn @(U32 -> U32 -> U32)    "add_uint32"  $ \x y -> x + y

add_int256  = fn @(I256 -> I256 -> I256) "add_int256"  $ \x y -> x + y
add_int128  = fn @(I128 -> I128 -> I128) "add_int128"  $ \x y -> x + y
add_int32   = fn @(I32 -> I32 -> I32)    "add_int32"   $ \x y -> x + y

sub_uint256 = fn @(U256 -> U256 -> U256) "sub_uint256" $ \x y -> x - y
sub_uint128 = fn @(U128 -> U128 -> U128) "sub_uint128" $ \x y -> x - y

sub_int256  = fn @(I256 -> I256 -> I256) "sub_int256"  $ \x y -> x - y
sub_int128  = fn @(I128 -> I128 -> I128) "sub_int128"  $ \x y -> x - y

object :: YulObject
object = mkYulObject "NumTests" emptyCtor
  [ pureFn add_uint256
  , pureFn add_uint128
  , pureFn add_uint32
  --
  , pureFn add_int256
  , pureFn add_int128
  , pureFn add_int32
  --
  , pureFn sub_uint256
  , pureFn sub_uint128
  --
  , pureFn sub_int256
  , pureFn sub_int128
  ]
