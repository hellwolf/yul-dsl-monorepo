{-# LANGUAGE DataKinds #-}

{-
-- |
-- Module      :  Data.TypeBools
-- License     :  LGPL-3
-- Maintainer: :  zhicheng.miao@gmail.com
-- Stability   :  experimental
-- Portability :  not portable
--
-- Additional singletons to work with type-level booleans in addition to the 'Data.Type.Bool' module.
--
-}

module Data.TypeBools where

-- | Boolean type singleton.
data SBool (s :: Bool) = SBool

-- | Known boolean singletons.
class KnownBool (s :: Bool) where
  toBool :: SBool s -> Bool
instance KnownBool True  where toBool _ = True
instance KnownBool False where toBool _ = False
