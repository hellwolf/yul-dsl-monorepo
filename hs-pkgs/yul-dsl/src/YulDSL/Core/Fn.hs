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

type Fn f = FnNP (UncurryNP'Fst f) (UncurryNP'Snd f)

data AnyFn = forall a b. YulO2 a b => MkAnyFn (FnCat a b)

deriving instance Show AnyFn

{- fn -}

class UncurriableFn f as xs b where
  -- | TODO
  uncurryFn :: f -> YulCat as xs -> YulCat as b

-- uncurry (b)
instance forall as x. UncurriableFn (YulCat (NP as) x) (NP as) (NP '[]) x where
  uncurryFn x _ = x

-- uncurry (x -> x' -> ...xs' -> b)
instance forall as x xs b g.
         ( YulO3 (NP as) x (NP xs)
         , UncurriableFn g (NP as) (NP xs) b
         ) => UncurriableFn (YulCat (NP as) x -> g) (NP as) (NP (x:xs)) b where
  uncurryFn f xxs = uncurryFn (f x) xs
    where xxs' = xxs >.> YulSplit
          x   = xxs' >.> YulExl
          xs  = xxs' >.> YulExr

fn :: forall f as b f'.
      ( YulO2 (NP as) b
      , as ~ UncurryNP'Fst f
      , b  ~ UncurryNP'Snd f
      , f' ~ LiftFunction f (YulCat (NP as)) Many
      , UncurriableFn f' (NP as) (NP as) b
      )
   => String -> f' -> Fn f
fn fid f = let cat = uncurryFn f (YulId @(NP as))
           in MkFn fid cat

{- callFn (!*) -}

-- | TODO
class ConstructibleNP as x xs where
  consNP :: YulCat (NP as) x -> YulCat (NP as) (NP xs) -> YulCat (NP as) (NP (x:xs))

-- buildNP (x -> (x))
instance forall as x.
         ( YulO2 (NP as) x
         ) => ConstructibleNP as x '[] where
  consNP x _ = x >.> YulCoerce

-- buildNP (x -> xs -> (x, ...xs)
instance forall as x x' xs'.
         ( YulO4 (NP as) x x' (NP xs')
         , ConstructibleNP as x' xs'
         ) => ConstructibleNP as x (x':xs') where
  consNP x as = YulFork x (consNP x' xs')
                 >.> YulCoerce
    where ass = as >.> YulSplit
          x'  = ass >.> YulExl
          xs' = ass >.> YulExr



-- ap'vfn :: forall a b r. (YulCatReducible a, YulO3 a b r)
--       => Fn a b -> AtomizeNP (YulCat r a) -> YulCat r b

-- ap'vfn fn a = YulJump (fnId fn) (fnCat fn) `YulComp` yul_cat_merge @a a
