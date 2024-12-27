{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestCommon where

import Data.Functor         ((<&>))
import Data.Maybe           (fromJust)
-- quickcheck
import Test.QuickCheck

import Ethereum.ContractABI

instance Arbitrary ADDR where
  arbitrary = chooseBoundedIntegral (minBound @U256, maxBound @U256)
              <&> fromJust . toAddr . toInteger

deriving newtype instance Arbitrary BOOL

instance ValidINTx s n => Arbitrary (INTx s n) where
  arbitrary = chooseBoundedIntegral (minBound, maxBound)
