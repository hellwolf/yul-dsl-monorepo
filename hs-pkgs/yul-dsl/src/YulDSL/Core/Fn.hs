{-# LANGUAGE AllowAmbiguousTypes #-}
module YulDSL.Core.Fn
  ( FnCat (MkFn), FnNP, Fn, fnId, fnCat, AnyFn (MkAnyFn)
  , fn
  ) where

import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCat


{- Fn Family -}

data FnCat a b where
  MkFn :: forall a b. YulO2 a b => { fnId :: String, fnCat :: YulCat a b } -> FnCat a b

instance YulO2 a b => Show (FnCat a b) where show (MkFn _ cat) = show cat

type FnNP as b = FnCat (NP as) b

type Fn f = FnNP (UncurryNP'L f) (UncurryNP'R f)

data AnyFn = forall a b. YulO2 a b => MkAnyFn (FnCat a b)

deriving instance Show AnyFn

{- fn -}

fn :: forall f as b f'.
      ( YulO2 (NP as) b
      , as ~ UncurryNP'L f
      , b  ~ UncurryNP'R f
      , f' ~ LiftFunction f (YulCat (NP as)) Many
      , UncurriableNP f' (YulCat (NP as)) as b Many
      )
   => String -> f' -> Fn f
fn fid f = let b = uncurryNP f (YulId @(NP as))
           in MkFn fid b

{- callFn (!*) -}


-- ap'vfn :: forall a b r. (YulCatReducible a, YulO3 a b r)
--       => Fn a b -> AtomizeNP (YulCat r a) -> YulCat r b

-- ap'vfn fn a = YulJump (fnId fn) (fnCat fn) `YulComp` yul_cat_merge @a a
