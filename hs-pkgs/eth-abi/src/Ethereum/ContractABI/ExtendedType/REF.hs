{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : hellwolf@yolc.dev
Stability   : experimental
Portability : GHC2024

= Description

References:

- https://docs.soliditylang.org/en/v0.8.28/internals/layout_in_storage.html

|-}
--
module Ethereum.ContractABI.ExtendedType.REF
  ( REF
  , ValidSlot, ValidOffset, constRef
  ) where
-- base
import GHC.TypeLits
--
import Ethereum.ContractABI.ABITypeable


-- | A storage or memory reference to type @a@ at the solidity conventional "(slot, offset)".
newtype REF a = REF (Integer, Integer)

-- | Each slot uses 32 bytes
type ValidSlot n = (KnownNat n, n <= (2 ^ 248))

-- | The offset within a slot must leave sufficient room to store type @a@.
type ValidOffset a n = (KnownNat n, ABITypeValueSize a + n <= 32)

-- | Create a reference at a slot and an offset value.
constRef :: forall a. forall slot -> forall offset -> (ValidSlot slot, ValidOffset a offset) => REF a
constRef slot offset = REF ( fromSNat (SNat @slot)
                           , fromSNat (SNat @offset) )

-- | Create a reference at keccak256 of a location string and an offset value.
-- refLocation :: forall a. String -> forall offset -> ValidOffset a offset => REF a
-- refLocation loc offset = REF
--                          -- truncate the last byte
--                          ( bytesnToInteger @31 . bytesnFromWord8s . drop 1 . unBYTESn $ stringKeccak256  loc
--                          , fromSNat (SNat @offset) )
