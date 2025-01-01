{-
Test code generations for typical and problematic functions.
-}
module Basic_Tests where
import Prelude.YulDSL

embUnit'p = fn @(I256 -> ()) "embUnit$p" $ \_ -> YulEmb ()

embTrue'p = fn @(BOOL) "embTrue$p" $ YulEmb true

embTrue'l = lfn "embTrue$l" $ yulmonad'p @(BOOL) (embed true)

revertIfTrue = fn @(BOOL -> U256 -> U256) "revertIfTrue"
  $ \t x -> if t then yulRevert else x

-- Test function recursion; but it will reach stack limit of EVM.
rangeSum'p = fn @(U256 -> U256 -> U256 -> U256) "rangeSum$p"
  \from step until -> let j = from + step
                      in from + if j <= until
                                then callFn rangeSum'p j step until
                                else 0

rangeSum'l = lfn "rangeSum$l" $
  yulmonad'p @(U256 -> U256 -> U256 -> U256)
  \from'p step'p until'p -> LVM.do
  impure $ callFn'l rangeSum'p from'p step'p until'p

object = mkYulObject "BasicTests" emptyCtor
  [ pureFn embUnit'p
  , pureFn embTrue'p
  , pureFn revertIfTrue
  , staticFn embTrue'l
  , pureFn rangeSum'p
  , staticFn rangeSum'l
  ]
