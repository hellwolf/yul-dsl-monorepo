{-# OPTIONS_GHC -Wno-orphans #-}

module TestCommon where

import           Data.Functor    ((<&>))
import           Data.Typeable   (Typeable)
import           GHC.Natural     (naturalFromInteger)
import           GHC.TypeNats    (KnownNat)

import           Test.QuickCheck

import           LoliYul.Core

instance Arbitrary ADDR where
  arbitrary = chooseBoundedIntegral (minBound @UINT256, maxBound @UINT256)
              <&> toInteger <&> naturalFromInteger <&> to_addr'

instance (Typeable s, KnownNat n) => Arbitrary (INTx s n) where
  arbitrary = chooseBoundedIntegral (minBound, maxBound)
