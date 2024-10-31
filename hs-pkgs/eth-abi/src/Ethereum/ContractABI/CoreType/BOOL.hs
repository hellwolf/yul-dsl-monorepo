{-# LANGUAGE TypeFamilies #-}

{-|

Module      : Ethereum.ContractABI.CoreType.BOOL
Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : GHC2024

-}

module Ethereum.ContractABI.CoreType.BOOL
  ( module Data.TypeBools
  , BOOL (BOOL), true, false
  ) where

import           Data.TypeBools
import           Ethereum.ContractABI.ABICoreType (ABICoreType (BOOL'), ABIWordValue (..), word, wordVal)
import           Ethereum.ContractABI.ABITypeable (ABITypeable (..))


-- | ABI boolean value type.
newtype BOOL = BOOL Bool deriving newtype (Eq)

-- | True value for 'BOOL'.
true :: BOOL
true = BOOL True

-- | False value for 'BOOL'.
false :: BOOL
false = BOOL False

instance ABITypeable BOOL where
  type instance ABITypeDerivedOf BOOL = BOOL
  abiTypeInfo = [BOOL']

instance Bounded BOOL where
  minBound = false
  maxBound = true

instance ABIWordValue BOOL where
  fromWord w = case wordVal w of
    0 -> Just false
    1 -> Just true
    _ -> Nothing

  toWord (BOOL False) = word 0
  toWord (BOOL True)  = word 1

instance Show BOOL where
  show (BOOL True)  = "true"
  show (BOOL False) = "false"
