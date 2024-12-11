{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : hellwolf@yolc.dev
Stability   : experimental
Portability : GHC2024

= Description

Ethereum contract ABI address type.

-}
module Ethereum.ContractABI.CoreType.ADDR
  ( ADDR
  , zeroAddress, MAX_ADDR, maxAddr
  , constAddr, toAddr
  ) where

-- base
import           GHC.TypeLits                      (KnownNat, fromSNat, type (-), type (<=), type (^))
import           Numeric                           (showHex)
-- cereal
import qualified Data.Serialize                    as S
--
import           Ethereum.ContractABI.ABICoreType
import           Ethereum.ContractABI.ABITypeable
import           Ethereum.ContractABI.ABITypeCodec

newtype ADDR = ADDR Integer deriving newtype (Ord, Eq)

-- | The proverbial zero address.
zeroAddress :: ADDR
zeroAddress = ADDR 0

type MAX_ADDR = (2 ^ 256) - 1

-- | Maximum possible value of address.
maxAddr :: ADDR
maxAddr = ADDR (fromSNat (natSing @MAX_ADDR))

constAddr :: forall (a :: Nat) -> (KnownNat a , a <= MAX_ADDR)
          => ADDR
constAddr a = ADDR (fromSNat (natSing @a))

toAddr :: Integer -> Maybe ADDR
toAddr a = if a >= 0 && a <= (fromSNat (natSing @MAX_ADDR)) then Just (ADDR a) else Nothing

instance Bounded ADDR where
  minBound = zeroAddress
  maxBound = maxAddr

instance ABITypeable ADDR where
  type instance ABITypeDerivedOf ADDR = ADDR
  abiTypeInfo = [ADDR']

instance ABITypeCodec ADDR where
  abiEncoder (ADDR x) = S.put x
  abiDecoder = fmap ADDR S.get

instance ABIWordValue ADDR where
  fromWord = toAddr . wordVal
  toWord (ADDR a) = word a

instance Show ADDR where
  -- TODO: show ERC-55 form
  show (ADDR a) = "0x" ++ showHex a ""
