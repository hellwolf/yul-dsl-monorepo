{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestCommon where

import           Data.Functor                  ((<&>))
import           Data.Typeable                 (Typeable)

import           Test.QuickCheck

import           YulDSL.Core.ContractABI.Types

instance Arbitrary ADDR where
  arbitrary = chooseBoundedIntegral (minBound @UINT256, maxBound @UINT256)
              <&> toInteger <&> to_addr'

deriving newtype instance Arbitrary BOOL

-- deriving instance Arbitrary BYTES

instance (Typeable s, KnownNat n) => Arbitrary (INTx s n) where
  arbitrary = chooseBoundedIntegral (minBound, maxBound)
