{-
Test code generations for typical and problematic functions.
-}
module Basic_Tests where
import Prelude.YulDSL

embUnit'p = fn @(I256 -> ()) "embUnit$p" $ \_ -> YulEmb ()

embTrue'p = fn @(BOOL) "embTrue$p" $ YulEmb true

embTrue'l = fn'l "embTrue$l" $ yulmonad'lp @(BOOL) (embed true)

revertIfTrue = fn @(BOOL -> U256 -> U256) "revertIfTrue"
  $ \t x -> if t then yulRevert else x

-- Test function recursion; but it will reach stack limit of EVM.
rangeSum'p = fn @(U256 -> U256 -> U256 -> U256) "rangeSum$p"
  \from step until -> let j = from + step
                      in from + if j <= until
                                then call'p rangeSum'p j step until
                                else 0

-- rangeSum'l = fn'l "rangeSum$l" $
--   uncurry'lp @(U256 -> U256 -> U256 -> U256)
--   \from'p step'p until'p -> LVM.do
--   <- call'l rangeSumP from'p step'p until'p

object = mkYulObject "BasicTests" emptyCtor
  [ pureFn embUnit'p
  , pureFn embTrue'p
  , pureFn revertIfTrue
  , pureFn (purify'l embTrue'l)
  , pureFn rangeSum'p
  ]
