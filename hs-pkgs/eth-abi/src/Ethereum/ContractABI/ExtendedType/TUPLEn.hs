{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ethereum.ContractABI.ExtendedType.TUPLEn where

import           Ethereum.ContractABI.ABITypeable (ABITypeable (ABITypeDerivedOf, abiFromCoreType, abiToCoreType))
import           Ethereum.ContractABI.CoreType.NP (NP (..))

{- AUTO GENERATED TupleN for N = [3..64]

@
import Data.List

genNPToTUpleNLine n =
  "  NPToTupleN (NP '[" <> ntuple <> "]) = (" <> ntuple <> ")"
  where ntuple = intercalate "," $ fmap (("a" <>) . show) [1..n]

gen n =
  "instance (" <> (intercalate "," (fmap (("ABITypeable a" <>) . show) [1..n])) <> ") =>\n" <>
  " ABITypeable (" <> ntuple <> ") where\n" <>
  "  type ABITypeDerivedOf (" <> ntuple <> ") = NP '[" <> ntuple <> "]\n" <>
  "  abiToCoreType(" <> ntuple <> ")=" <> nprod <> "\n" <>
  "  abiFromCoreType(" <> nprod <> ")=(" <> ntuple <> ")"
  where ntuple = intercalate "," $ fmap (("a" <>) . show) [1..n]
        nprod = intercalate ":*" $ fmap (("a" <>) . show) [1..n] <> ["Nil"]

main = do
  mapM (putStrLn . gen) [3..8]
  mapM (putStrLn . genNPToTUpleNLine) [3..8]
@
-}

type family NPToTupleN (np) where
  NPToTupleN (NP '[]) = ()
  NPToTupleN (NP '[a]) = a
  NPToTupleN (NP '[a1,a2]) = (a1,a2)
  NPToTupleN (NP '[a1,a2,a3]) = (a1,a2,a3)
  NPToTupleN (NP '[a1,a2,a3,a4]) = (a1,a2,a3,a4)
  NPToTupleN (NP '[a1,a2,a3,a4,a5]) = (a1,a2,a3,a4,a5)
  NPToTupleN (NP '[a1,a2,a3,a4,a5,a6]) = (a1,a2,a3,a4,a5,a6)
  NPToTupleN (NP '[a1,a2,a3,a4,a5,a6,a7]) = (a1,a2,a3,a4,a5,a6,a7)
  NPToTupleN (NP '[a1,a2,a3,a4,a5,a6,a7,a8]) = (a1,a2,a3,a4,a5,a6,a7,a8)

instance (ABITypeable a1,ABITypeable a2,ABITypeable a3) =>
 ABITypeable (a1,a2,a3) where
  type ABITypeDerivedOf (a1,a2,a3) = NP '[a1,a2,a3]
  abiToCoreType(a1,a2,a3)=a1:*a2:*a3:*Nil
  abiFromCoreType(a1:*a2:*a3:*Nil)=(a1,a2,a3)
instance (ABITypeable a1,ABITypeable a2,ABITypeable a3,ABITypeable a4) =>
 ABITypeable (a1,a2,a3,a4) where
  type ABITypeDerivedOf (a1,a2,a3,a4) = NP '[a1,a2,a3,a4]
  abiToCoreType(a1,a2,a3,a4)=a1:*a2:*a3:*a4:*Nil
  abiFromCoreType(a1:*a2:*a3:*a4:*Nil)=(a1,a2,a3,a4)
instance (ABITypeable a1,ABITypeable a2,ABITypeable a3,ABITypeable a4,ABITypeable a5) =>
 ABITypeable (a1,a2,a3,a4,a5) where
  type ABITypeDerivedOf (a1,a2,a3,a4,a5) = NP '[a1,a2,a3,a4,a5]
  abiToCoreType(a1,a2,a3,a4,a5)=a1:*a2:*a3:*a4:*a5:*Nil
  abiFromCoreType(a1:*a2:*a3:*a4:*a5:*Nil)=(a1,a2,a3,a4,a5)
instance (ABITypeable a1,ABITypeable a2,ABITypeable a3,ABITypeable a4,ABITypeable a5,ABITypeable a6) =>
 ABITypeable (a1,a2,a3,a4,a5,a6) where
  type ABITypeDerivedOf (a1,a2,a3,a4,a5,a6) = NP '[a1,a2,a3,a4,a5,a6]
  abiToCoreType(a1,a2,a3,a4,a5,a6)=a1:*a2:*a3:*a4:*a5:*a6:*Nil
  abiFromCoreType(a1:*a2:*a3:*a4:*a5:*a6:*Nil)=(a1,a2,a3,a4,a5,a6)
instance (ABITypeable a1,ABITypeable a2,ABITypeable a3,ABITypeable a4,ABITypeable a5,ABITypeable a6,ABITypeable a7) =>
 ABITypeable (a1,a2,a3,a4,a5,a6,a7) where
  type ABITypeDerivedOf (a1,a2,a3,a4,a5,a6,a7) = NP '[a1,a2,a3,a4,a5,a6,a7]
  abiToCoreType(a1,a2,a3,a4,a5,a6,a7)=a1:*a2:*a3:*a4:*a5:*a6:*a7:*Nil
  abiFromCoreType(a1:*a2:*a3:*a4:*a5:*a6:*a7:*Nil)=(a1,a2,a3,a4,a5,a6,a7)
instance (ABITypeable a1,ABITypeable a2,ABITypeable a3,ABITypeable a4,ABITypeable a5,ABITypeable a6,ABITypeable a7,ABITypeable a8) =>
 ABITypeable (a1,a2,a3,a4,a5,a6,a7,a8) where
  type ABITypeDerivedOf (a1,a2,a3,a4,a5,a6,a7,a8) = NP '[a1,a2,a3,a4,a5,a6,a7,a8]
  abiToCoreType(a1,a2,a3,a4,a5,a6,a7,a8)=a1:*a2:*a3:*a4:*a5:*a6:*a7:*a8:*Nil
  abiFromCoreType(a1:*a2:*a3:*a4:*a5:*a6:*a7:*a8:*Nil)=(a1,a2,a3,a4,a5,a6,a7,a8)
