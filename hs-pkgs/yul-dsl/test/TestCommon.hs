{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestCommon where

import Data.Functor         ((<&>))
-- quickcheck
import Test.QuickCheck

import Ethereum.ContractABI

instance Arbitrary ADDR where
  arbitrary = chooseBoundedIntegral (minBound @U160, maxBound @U160)
              <&> addrFromU160

deriving newtype instance Arbitrary BOOL

instance ValidINTx s n => Arbitrary (INTx s n) where
  arbitrary = chooseBoundedIntegral (minBound, maxBound)
