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
  ) where

-- | Boolean type singleton.
data SBool (s :: Bool) = SBool

-- | Known boolean singletons.
class KnownBool (s :: Bool) where
  toBool :: SBool s -> Bool
  toBool' :: Bool
instance KnownBool True  where toBool _ = True; toBool' = True;
instance KnownBool False where toBool _ = False; toBool' = False;
