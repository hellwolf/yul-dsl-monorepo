{-# LANGUAGE AllowAmbiguousTypes #-}

{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : hellwolf@yolc.dev
Stability   : experimental
Portability : GHC2024

= Description

Boolean singleton 'SBool' to work with type-level booleans in addition to the 'Data.Type.Bool' module.

-}
module Internal.Data.Type.Bool
  ( SBool (SBool)
  , KnownBool (..)
  , TypeEq
  ) where

-- | Boolean type singleton.
data SBool (s :: Bool) = SBool

-- | Known boolean singletons.
class KnownBool (s :: Bool) where
  toBool :: SBool s -> Bool
  toBool' :: Bool
instance KnownBool True  where toBool _ = True; toBool' = True;
instance KnownBool False where toBool _ = False; toBool' = False;

-- | Test type equality using overlapping type family instances.
--
-- Note: https://typesandkinds.wordpress.com/2012/12/22/ordered-overlapping-type-family-instances/
type family TypeEq (a :: k) (b :: k) :: Bool where
  TypeEq a a = True
  TypeEq _ _ = False
