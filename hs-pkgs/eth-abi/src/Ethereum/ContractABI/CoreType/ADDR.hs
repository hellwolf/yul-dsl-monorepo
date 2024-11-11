{-# LANGUAGE TypeFamilies #-}

{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : GHC2024

= Description

Ethereum contract ABI address type.

-}
module Ethereum.ContractABI.CoreType.ADDR
  ( ADDR, zeroAddress, maxAddr, toAddr
  ) where

-- base
-- import GHC.TypeLits (fromSNat, KnownNat(natSing), type (^), type (-), type (<=?) )
-- import Data.Type.Bool ( type (&&) )
import Numeric (showHex)
--
import Ethereum.ContractABI.ABITypeable
import Ethereum.ContractABI.ABICoreType

newtype ADDR = ADDR Integer deriving newtype (Ord, Eq)

-- | The proverbial zero address.
zeroAddress :: ADDR
zeroAddress = ADDR 0

-- | Maximum possible value of address.
maxAddr :: ADDR
maxAddr = ADDR max_addr_nat

-- toAddr :: forall n -> (KnownNat n, (0 <=? n && n <=? 2 ^ 256 - 1) ~ True) => ADDR
-- toAddr a = ADDR (fromSNat (natSing @a))

toAddr :: Integer -> Maybe ADDR
toAddr a = if a >= 0 && a <= max_addr_nat then Just (ADDR a) else Nothing

instance ABITypeable ADDR where
  type instance ABITypeDerivedOf ADDR = ADDR
  abiTypeInfo = [ADDR']

instance Bounded ADDR where
  minBound = zeroAddress
  maxBound = maxAddr

instance ABIWordValue ADDR where
  fromWord = toAddr . wordVal
  toWord (ADDR a) = word a

instance Show ADDR where
  -- TODO: show ERC-55 form
  show (ADDR a) = "0x" ++ showHex a ""

{- INTERNAL FUNCTIONS -}

max_addr_nat :: Integer
max_addr_nat = (2 ^ (256 :: Integer)) - 1
