module Num_Tests where

import           Prelude.YulDSL

add_uint256 = fn @(U256 -> U256 -> U256) "add_uint256" $ \x y -> x + y
add_uint128 = fn @(U128 -> U128 -> U128) "add_uint128" $ \x y -> x + y
add_int256  = fn @(I256 -> I256 -> I256) "add_int256"  $ \x y -> x + y
add_int128  = fn @(I128 -> I128 -> I128) "add_int128"  $ \x y -> x + y

sub_uint256 = fn @(U256 -> U256 -> U256) "sub_uint256" $ \x y -> x - y
sub_uint128 = fn @(U128 -> U128 -> U128) "sub_uint128" $ \x y -> x - y
sub_int256  = fn @(I256 -> I256 -> I256) "sub_int256"  $ \x y -> x - y
sub_int128  = fn @(I128 -> I128 -> I128) "sub_int128"  $ \x y -> x - y

object :: YulObject
object = mkYulObject "NumTests" emptyCtor
  [ externalFn add_uint256
  , externalFn add_uint128
  , externalFn add_int256
  , externalFn add_int128
  , externalFn sub_uint256
  , externalFn sub_uint128
  , externalFn sub_int256
  , externalFn sub_int128
  ]
