{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestCommon where

import           Data.Functor                  ((<&>))

import           Test.QuickCheck

import           YulDSL.Core.ContractABI.Types

instance Arbitrary ADDR where
  arbitrary = chooseBoundedIntegral (minBound @UINT256, maxBound @UINT256)
              <&> to_addr' . toInteger

deriving newtype instance Arbitrary BOOL

-- deriving instance Arbitrary BYTES

instance (KnownBool s, KnownNat n) => Arbitrary (INTx s n) where
  arbitrary = chooseBoundedIntegral (minBound, maxBound)
