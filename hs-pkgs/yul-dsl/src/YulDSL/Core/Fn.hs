{-# LANGUAGE AllowAmbiguousTypes #-}

module YulDSL.Core.Fn
  ( FnCat (MkFnCat), FnNP, Fn (MkFn, unFn), fnId, fnCat
  , PureFn
  , AnyFn (MkAnyFn)
  , curry'p, fn, call'p
  ) where

-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCat


{- * Fn and Its Aliases -}

-- | Yul functions are encoded in their categorical forms.
data FnCat eff a b where
  -- | Create a yul function from its unique id and morphism from @a@ to @b@.
  MkFnCat :: forall eff a b. YulO2 a b
          => { fnId  :: String         -- ^ the unique id of the yul function
             , fnCat :: YulCat eff a b -- ^ the morphism of the yul function
             } -> FnCat eff a b

instance YulO2 a b => Show (FnCat eff a b) where show (MkFnCat _ cat) = show cat

-- | Yul functions that have their arguments in 'NP' forms.
type FnNP eff xs b = FnCat eff (NP xs) b

-- | Yul functions that denoted in currying function forms.
--
--   Note: Fn (a1 -> a2 -> ...aN -> b) ~ FnNP (NP [a1,a2...aN]) b
newtype Fn eff f = MkFn { unFn :: FnNP eff (UncurryNP'Fst f) (UncurryNP'Snd f) }

-- | Function without effects, hence pure.
type PureFn = Fn MkPure

-- | Existential type for any @FnCat a b@.
data AnyFn = forall eff a b. YulO2 a b => MkAnyFn (FnCat eff a b)

deriving instance Show AnyFn

{-* fn -}

curry'p :: forall f xs b f' m.
           ( YulO2 (NP xs) b
           , UncurryNP'Fst f ~ xs
           , UncurryNP'Snd f ~ b
           , YulCat MkPure (NP xs) ~ m
           , LiftFunction f m m Many ~ f'
           , UncurryingNP f xs b m m m m Many
           )
        => f' -> YulCat MkPure (NP xs) b
curry'p f = uncurryingNP @f @xs @b @m @m @m @m f YulId

fn :: forall f xs b f' m.
      ( YulO2 (NP xs) b
      , UncurryNP'Fst f ~ xs
      , UncurryNP'Snd f ~ b
      , YulCat MkPure (NP xs) ~ m
      , LiftFunction f m m Many ~ f'
      , UncurryingNP f xs b m m m m Many
      )
   => String -> f' -> PureFn f
fn fid f = MkFn (MkFnCat fid (curry'p @f f))

-- fn fid f = let cat = uncurryingNP @f @xs @b @(YulCat (NP xs)) @(YulCat (NP xs)) f (YulId @(NP xs))
--            in MkFn (MkFnCat fid cat)

{-* callFn -}

call'p :: forall f xs b f' r m.
          ( YulO3 (NP xs) b r
          , UncurryNP'Fst f ~ xs
          , UncurryNP'Snd f ~ b
          , CurryNP (NP xs) b ~ f
          , m ~ YulCat MkPure r
          , LiftFunction f m m Many ~ f'
          , CurryingNP xs b m m m Many
          )
       => PureFn f -> f'
call'p (MkFn f) = curryingNP @xs @b @m @m @m @Many
                  (\xs -> xs >.> YulJump (fnId f) (fnCat f))
