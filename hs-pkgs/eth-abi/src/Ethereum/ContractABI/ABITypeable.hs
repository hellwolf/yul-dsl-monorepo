{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE TypeFamilies        #-}

{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : GHC2024

= Description

'ABITypeable' is the required type class for extended types.

-}
module Ethereum.ContractABI.ABITypeable
 ( ABITypeable (..)
 , AnyABITypeable (MkAnyABITypeable)
 , AnyABITypeDerivedOf (MkAnyABIDerivedType)
 , abiTypeCanonName, abiTypeCompactName
 ) where

import           Data.Constraint                  (Dict)
import           Data.List                        (intercalate)
--
import           Ethereum.ContractABI.ABICoreType (ABICoreType (..), abiCoreTypeCanonName, abiCoreTypeCompactName)


-- | Type information for all core and derived contract ABI types.
class Show a => ABITypeable a where
  -- | Convert @a@ to the ABI core type it derives from.
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

-- | Existential type of all abi types.
data AnyABITypeable = forall a. ABITypeable a => MkAnyABITypeable a

instance Show AnyABITypeable where
  show (MkAnyABITypeable a) = show a

-- | Existential type of all abi types that derive from the same core type @c@.
data AnyABITypeDerivedOf c = forall a. (ABITypeable a, c ~ ABITypeDerivedOf a) => MkAnyABIDerivedType a

-- | Maybe types of any abi type is also an abi type.
instance ABITypeable a => ABITypeable (Maybe a) where
  type instance ABITypeDerivedOf (Maybe a) = Maybe a
  abiTypeInfo = fmap MAYBE' (abiTypeInfo @a)

-- | Canonical name of the type that is used for computing the function selector.
abiTypeCanonName :: forall a. ABITypeable a => String
abiTypeCanonName = intercalate "," (fmap abiCoreTypeCanonName (abiTypeInfo @a))

-- | A 'abiTypeCanonName' variant that is compact to saving characters.
abiTypeCompactName :: forall a. ABITypeable a => String
abiTypeCompactName = intercalate "" (fmap abiCoreTypeCompactName (abiTypeInfo @a))
