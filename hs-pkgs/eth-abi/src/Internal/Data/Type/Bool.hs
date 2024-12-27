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
  ( SBool
  , KnownBool (boolSing, fromSBool, fromBoolKind), toKnownSBool
  , module Data.Type.Bool
  ) where
import Data.Type.Bool


-- | Boolean type singleton.
data SBool (s :: Bool) = SBool

-- | Known boolean singletons.
class KnownBool (s :: Bool) where
  -- | From a singleton Bool to Bool.
  fromSBool :: SBool s -> Bool
  -- | Create a singleton Bool from a Bool kind. It is named similarly to natSing.
  boolSing :: SBool s
  boolSing = SBool
  -- | Demote the Bool kind to a Bool value.
  fromBoolKind :: Bool
  fromBoolKind = fromSBool (boolSing @s)

instance KnownBool True  where fromSBool _ = True
instance KnownBool False where fromSBool _ = False

toKnownSBool :: Bool -> (forall s. KnownBool s => SBool s -> a) -> a
toKnownSBool True f  = f (boolSing @True)
toKnownSBool False f = f (boolSing @False)
