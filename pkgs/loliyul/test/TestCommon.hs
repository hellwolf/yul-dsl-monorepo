{-# OPTIONS_GHC -Wno-orphans #-}

module TestCommon where

-- import           Data.Functor    ((<&>))
import           Data.Typeable   (Typeable)
import           GHC.TypeNats    (KnownNat)

import           Test.QuickCheck

import           LoliYul.Core

instance (Typeable s, KnownNat n) => Arbitrary (INTx s n) where
  arbitrary = chooseBoundedIntegral (minBound, maxBound)
