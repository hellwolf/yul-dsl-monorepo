{-
Test code generations for typical and problematic functions.
-}
module Basic_Tests where
import Prelude.YulDSL

embUnit'p = fn @(I256 -> ()) $locId \_ -> YulEmb ()

embTrue'p = fn @(BOOL) $locId $ YulEmb true

embTrue'l = lfn $locId $ yulmonad'p @(BOOL) (embed true)

revertIfTrue = fn @(BOOL -> U256 -> U256) $locId
  $ \t x -> if t then yulRevert else x

-- Test function recursion; but it will reach stack limit of EVM.
rangeSum'p = fn @(U256 -> U256 -> U256 -> U256) $locId
  \from step until -> let j = from + step
                      in from + if j <= until
                                then callFn rangeSum'p j step until
                                else 0

rangeSum'l = lfn $locId $
  yulmonad'p @(U256 -> U256 -> U256 -> U256)
  \from'p step'p until'p -> LVM.do
  impure $ callFn'lpp rangeSum'p from'p step'p until'p

object = mkYulObject "BasicTests" emptyCtor
  [ pureFn   "embUnit$p" embUnit'p
  , pureFn   "embTrue$p" embTrue'p
  , pureFn   "revertIfTrue" revertIfTrue
  , staticFn "embTrue$l" embTrue'l
  , pureFn   "rangeSum$p" rangeSum'p
  , staticFn "rangeSum$l" rangeSum'l
  ]
