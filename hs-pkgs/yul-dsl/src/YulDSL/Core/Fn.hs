{-# LANGUAGE AllowAmbiguousTypes #-}

module YulDSL.Core.Fn
  ( FnCat (MkFn), FnNP, Fn, fnId, fnCat, AnyFn (MkAnyFn)
  , fn
  ) where

import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCat


{- Fn and Its Aliases -}

-- | Yul functions are encoded in their categorical forms.
data FnCat a b where
  -- | Create a yul function from its unique id and morphism from @a@ to @b@.
  MkFn :: forall a b. YulO2 a b => { fnId  :: String       -- ^ the unique id of the yul function
                                   , fnCat :: YulCat a b  -- ^ the morphism of the yul function
                                   } -> FnCat a b

instance YulO2 a b => Show (FnCat a b) where show (MkFn _ cat) = show cat

-- | Yul functions that have their arguments in 'NP' forms.
type FnNP xs b = FnCat (NP xs) b

-- | Yul functions that denoted in currying function forms.
--
--   Note: Fn (a1 -> a2 -> ...aN -> b) ~ FnNP (NP [a1,a2...aN]) b
type Fn f = FnNP (UncurryNP'Fst f) (UncurryNP'Snd f)

-- | Existential type for any @FnCat a b@.
data AnyFn = forall a b. YulO2 a b => MkAnyFn (FnCat a b)

deriving instance Show AnyFn

{- fn -}

-- uncurry'v :: forall f as b f'.
--              ( YulO2 (NP as) b
--              , f' ~ LiftFunction f (YulCat (NP as)) Many
--              , as ~ UncurryNP'Fst f, b ~ UncurryNP'Snd f
--              , UncurriableNP f as b (YulCat (NP as)) (YulCat (NP as)) Many
--              ) => f' -> YulCat (NP as) b
-- uncurry'v f = uncurriableNP @f @as @b @(YulCat (NP as)) @(YulCat (NP as)) @Many f (YulId @(NP as))

fn :: forall f xs b f'.
      ( YulO2 (NP xs) b
      , xs ~ UncurryNP'Fst f
      , b  ~ UncurryNP'Snd f
      , f' ~ LiftFunction f (YulCat (NP xs)) Many
      , UncurryingNP f xs b (YulCat (NP xs)) (YulCat (NP xs)) Many
      )
   => String -> f' -> Fn f
fn fid f = let cat = uncurryingNP @f @xs @b @(YulCat (NP xs)) @(YulCat (NP xs)) f (YulId @(NP xs))
           in MkFn fid cat

-- class CallableFn (m1 :: Type -> Type) (p :: Multiplicity) where
--  callFn :: forall as b. FnNP as b %p-> m2 b

{- callFn (!*) -}

-- callFn :: forall as b.
--   (
--   )
--   => FnNP as b -> (YulCat (NP as)) b
-- callFn = _

-- ap'vfn :: forall a b r. (YulCatReducible a, YulO3 a b r)
--       => Fn a b -> AtomizeNP (YulCat r a) -> YulCat r b

-- ap'vfn fn a = YulJump (fnId fn) (fnCat fn) `YulComp` yul_cat_merge @a a
