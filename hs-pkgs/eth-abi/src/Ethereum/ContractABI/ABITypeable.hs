{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE TypeFamilies        #-}

module Ethereum.ContractABI.ABITypeable
 ( ABITypeable (..)
 , AnyABITypeDerivedOf (ABIDerivedType)
 ) where

import           Data.Constraint                  (Dict (..))
--
import           Ethereum.ContractABI.ABICoreType (ABICoreType)


-- | Type information for all core and derived contract ABI types.
class ABITypeable a where
  -- | Convert @a@ to the compatible ABI core type it derives from.
  type ABITypeDerivedOf a

  -- | Possible breakdown of the product object type.
  abiProdObjs :: forall b c. a ~ (b, c) => Dict (ABITypeable b, ABITypeable c)
  abiProdObjs = error "abi_prod_objs should only be implemented by the product of ABIType"

  -- | Returns a list of core types represented by this type.
  --
  -- Invariant: @abi_type_info a == abi_type_info \@(ABITypeDerivedOf a)@
  abiTypeInfo :: [ABICoreType]
  -- ^ The default implementation must be implemented by core types.
  default abiTypeInfo :: ABITypeable (ABITypeDerivedOf a) => [ABICoreType]
  abiTypeInfo = abiTypeInfo @(ABITypeDerivedOf a)

  abiToCoreType :: a -> ABITypeDerivedOf a
  default abiToCoreType :: a ~ ABITypeDerivedOf a => a -> ABITypeDerivedOf a
  abiToCoreType = id

  abiFromCoreType :: ABITypeDerivedOf a -> a
  default abiFromCoreType :: a ~ ABITypeDerivedOf a => ABITypeDerivedOf a -> a
  abiFromCoreType = id


data AnyABITypeDerivedOf c = forall a. ( ABITypeable a, c ~ ABITypeDerivedOf a
                                       ) => ABIDerivedType a

-- FIXME: REMOVE THESE
