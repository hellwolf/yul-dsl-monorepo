{-# LANGUAGE DataKinds #-}

{-|

Module      : Data.TypeBools
Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : GHC2024

= Description

Additional singletons to work with type-level booleans in addition to the 'Data.Type.Bool' module.

-}

module Data.TypeBools where

-- | Boolean type singleton.
data SBool (s :: Bool) = SBool

-- | Known boolean singletons.
class KnownBool (s :: Bool) where
  toBool :: SBool s -> Bool
instance KnownBool True  where toBool _ = True
instance KnownBool False where toBool _ = False
