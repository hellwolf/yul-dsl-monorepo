{-|

Copyright   : (c) 2023-2024 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : hellwolf@yolc.dev
Stability   : experimental

= Description

Function type for a YulCat.

-}
module YulDSL.Core.Fn
  ( FnCat (MkFnCat), FnNP, Fn (MkFn, unFn), fnId, fnCat
  , AnyFn (MkAnyFn)
  ) where

-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCat


{- * FnCat: yul function type and its aliases -}

-- | Yul functions are encoded in their categorical forms.
data FnCat eff a b where
  -- | Create a yul function from its unique id and morphism from @a@ to @b@.
  MkFnCat :: forall eff a b. YulO2 a b
          => { fnId  :: String         -- ^ the unique id of the yul function
             , fnCat :: YulCat eff a b -- ^ the morphism of the yul function
             }
          -> FnCat eff a b

instance YulO2 a b => Show (FnCat eff a b) where
  show (MkFnCat fid cat) = "fn " ++ fid ++ ":\n" ++ show cat

-- | Yul functions that have their arguments in 'NP' forms.
type FnNP eff xs b = FnCat eff (NP xs) b

-- | Yul functions that denoted in currying function forms.
--
--   Note: Fn (a1 -> a2 -> ...aN -> b) ~ FnNP (NP [a1,a2...aN]) b
newtype Fn eff f = MkFn { unFn :: FnNP eff (UncurryNP'Fst f) (UncurryNP'Snd f) }

{- * AnyFn -}

-- | Existential type for any @FnCat a b@.
data AnyFn = forall eff a b. YulO2 a b => MkAnyFn (FnCat eff a b)

deriving instance Show AnyFn
