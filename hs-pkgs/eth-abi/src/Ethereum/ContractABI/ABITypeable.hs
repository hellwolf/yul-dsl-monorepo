{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}

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

-- base
import           Data.Kind                        (Constraint, Type)
import           Data.List                        (intercalate)
--
import           Ethereum.ContractABI.ABICoreType


-- | Type information for all core and derived contract ABI types.
class ABITypeable a where
  -- | Convert @a@ to the ABI core type it derives from.
  type ABITypeDerivedOf a

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
data AnyABITypeable (c :: Type -> Constraint) = forall a. c a => MkAnyABITypeable a

instance Show (AnyABITypeable Show) where
  show (MkAnyABITypeable a) = show a

-- | Existential type of all abi types that derive from the same core type @c@.
data AnyABITypeDerivedOf c = forall a. (ABITypeable a, c ~ ABITypeDerivedOf a) => MkAnyABIDerivedType a

-- | Canonical name of the type that is used for computing the function selector.
abiTypeCanonName :: forall a. ABITypeable a => String
abiTypeCanonName = intercalate "," (fmap abiCoreTypeCanonName (abiTypeInfo @a))

-- | A 'abiTypeCanonName' variant that is compact to saving characters.
abiTypeCompactName :: forall a. ABITypeable a => String
abiTypeCompactName = intercalate "" (fmap abiCoreTypeCompactName (abiTypeInfo @a))
