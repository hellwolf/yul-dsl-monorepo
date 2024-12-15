{-|

Copyright   : (c) 2023-2024 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : hellwolf@yolc.dev
Stability   : experimental

= Description

Function types for YulCat.

-}
module YulDSL.Core.Fn
  ( -- * Raw yul functions.
    FnCat (MkFnCat, fnId, fnCat)
    -- * NP-formed yul functions.
  , FnNP, Fn (MkFn, unFn)
    -- * Existential type of yul functions.
  , AnyFnCat (MkAnyFnCat)
  ) where

-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCat
import           YulDSL.Core.YulCatObj


-- | Yul functions are encoded in their categorical forms.
data FnCat eff a b where
  -- | Create a yul function from its unique id and morphism from @a@ to @b@.
  MkFnCat :: forall eff a b. YulO2 a b
          => { fnId  :: String         -- ^ the unique id of the yul function
             , fnCat :: YulCat eff a b -- ^ the morphism of the yul function
             }
          -> FnCat eff a b

-- | Yul functions that have their arguments in 'NP' forms.
type FnNP eff xs b = FnCat eff (NP xs) b

-- | Yul function wrappers that are in currying function forms.
--
--   Note: Fn (a1 -> a2 -> ...aN -> b) ~ FnNP (NP [a1,a2...aN]) b
data Fn eff f where
  MkFn :: forall eff f xs b.
          ( UncurryNP'Fst f ~ xs,
            UncurryNP'Snd f ~ b,
            YulO2 (NP xs) b
          )
       => { unFn :: FnNP eff (UncurryNP'Fst f) (UncurryNP'Snd f) } -> Fn eff f

-- | Existential type for @FnCat a b@.
data AnyFnCat where
  MkAnyFnCat :: forall eff a b. YulO2 a b => (FnCat eff a b) -> AnyFnCat

--
-- Show instances
--

instance YulO2 a b => Show (FnCat eff a b) where
  show (MkFnCat fid cat) = "fn " ++ fid ++ ":\n" ++ show cat
deriving instance Show AnyFnCat
deriving instance Show (Fn eff f)
