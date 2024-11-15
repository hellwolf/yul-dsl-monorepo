{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE TypeFamilies        #-}

module YulDSL.Core.Fn
  ( FnCat (MkFnCat), FnNP, Fn (MkFn, unFn), fnId, fnCat, AnyFn (MkAnyFn)
  , fn
  , call
  ) where

-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCat


{- * Fn and Its Aliases -}

-- | Yul functions are encoded in their categorical forms.
data FnCat a b where
  -- | Create a yul function from its unique id and morphism from @a@ to @b@.
  MkFnCat :: forall a b. YulO2 a b => { fnId  :: String       -- ^ the unique id of the yul function
                                      , fnCat :: YulCat a b  -- ^ the morphism of the yul function
                                      } -> FnCat a b

instance YulO2 a b => Show (FnCat a b) where show (MkFnCat _ cat) = show cat

-- | Yul functions that have their arguments in 'NP' forms.
type FnNP xs b = FnCat (NP xs) b

-- | Yul functions that denoted in currying function forms.
--
--   Note: Fn (a1 -> a2 -> ...aN -> b) ~ FnNP (NP [a1,a2...aN]) b
newtype Fn f = MkFn { unFn :: FnNP (UncurryNP'Fst f) (UncurryNP'Snd f) }

-- | Existential type for any @FnCat a b@.
data AnyFn = forall a b. YulO2 a b => MkAnyFn (FnCat a b)

deriving instance Show AnyFn

{-* fn -}

fn :: forall f xs b f'.
      ( YulO2 (NP xs) b
      , UncurryNP'Fst f ~ xs
      , UncurryNP'Snd f ~ b
      , LiftFunction f (YulCat (NP xs)) Many ~ f'
      , UncurryingNP f xs b (YulCat (NP xs)) (YulCat (NP xs)) Many
      )
   => String -> f' -> Fn f
fn fid f = let cat = uncurryingNP @f @xs @b @(YulCat (NP xs)) @(YulCat (NP xs)) f (YulId @(NP xs))
           in MkFn (MkFnCat fid cat)

{-* callFn -}

call :: forall f xs b f' a.
        ( YulO3 (NP xs) b a
        , UncurryNP'Fst f ~ xs
        , UncurryNP'Snd f ~ b
        , CurryNP (NP xs) b ~ f
        , LiftFunction f (YulCat a) Many ~ f'
        , CurryingNP xs b (YulCat a) (YulCat a) Many
        )
     => Fn f -> f'
call (MkFn f) = curryingNP @xs @b @(YulCat a) @(YulCat a) @Many
                (\xs -> xs >.> YulJump (fnId f) (fnCat f))
