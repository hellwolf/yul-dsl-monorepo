{-# OPTIONS_GHC -Wno-orphans #-}
{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : hellwolf@yolc.dev
Stability   : experimental
Portability : GHC2024

= Description

Ethereum contract ABI compatible tuples encoded as simple n-ary products 'Data.SimpleNP.NP'.

-}
module Ethereum.ContractABI.CoreType.NP
  ( module Data.SimpleNP
  ) where

-- cereal
import Data.Serialize                    qualified as S
--
import Data.SimpleNP
--
import Ethereum.ContractABI.ABITypeable  (ABITypeable (..))
import Ethereum.ContractABI.ABITypeCodec (ABITypeCodec (..))


--
-- ABITypeable instances: NP xs
--

instance ABITypeable (NP '[]) where
  type instance ABITypeDerivedOf (NP '[]) = NP '[]
  type instance ABITypeValueSize (NP '[]) = 32
  abiTypeInfo = []

instance ( ABITypeable x, ABITypeable (NP xs)
         ) => ABITypeable (NP (x : xs)) where
  type instance ABITypeDerivedOf (NP (x : xs)) = NP (x : xs)
  type instance ABITypeValueSize (NP (x : xs)) = 32
  abiTypeInfo = abiTypeInfo @x <> abiTypeInfo @(NP xs)

instance ABITypeCodec (NP '[]) where
  abiEncoder Nil = S.put ()
  abiDecoder = S.get @() >> pure Nil

instance ( ABITypeable x, ABITypeCodec x, ABITypeCodec (NP xs)
         ) => ABITypeCodec (NP (x : xs)) where
  abiEncoder (x :* xs) = do
    abiEncoder x
    abiEncoder xs
  abiDecoder = do
    x <- abiDecoder
    xs <- abiDecoder
    pure (x :* xs)

--
-- ABITypeable instances: (), (a, b)
--

-- | ABI typeable unit.
instance ABITypeable () where
  type instance ABITypeDerivedOf () = NP '[]
  type instance ABITypeValueSize () = 0
  abiToCoreType () = Nil
  abiFromCoreType Nil = ()

-- | ABI typeable tuple.
instance (ABITypeable a1, ABITypeable a2) => ABITypeable (a1, a2) where
  type instance ABITypeDerivedOf (a1, a2) = NP '[a1, a2]
  type instance ABITypeValueSize (a1, a2) = 32 -- not a value type anymore
  abiToCoreType (x1, x2) = x1 :* x2 :* Nil
  abiFromCoreType (x1 :* x2 :* Nil) = (x1, x2)

instance ABITypeCodec () where

instance ( ABITypeCodec a1, ABITypeCodec a2
         ) => ABITypeCodec (a1, a2)
