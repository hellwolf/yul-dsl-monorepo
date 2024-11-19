{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ethereum.ContractABI.ExtendedType.TUPLEn where

-- import           Ethereum.ContractABI.ABITypeable (ABITypeable (..))
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
  "  type instance ABITypeDerivedOf (" <> ntuple <> ") = NP '[" <> ntuple <> "]\n" <>
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

-- instance (ABITypeable a1,ABITypeable a2,ABITypeable a3) =>
--  ABITypeable (a1,a2,a3) where
--   type instance ABITypeDerivedOf (a1,a2,a3) = NP '[a1,a2,a3]
--   abiToCoreType(a1,a2,a3)=a1:*a2:*a3:*Nil
--   abiFromCoreType(a1:*a2:*a3:*Nil)=(a1,a2,a3)
-- instance (ABITypeable a1,ABITypeable a2,ABITypeable a3,ABITypeable a4) =>
--  ABITypeable (a1,a2,a3,a4) where
--   type instance ABITypeDerivedOf (a1,a2,a3,a4) = NP '[a1,a2,a3,a4]
--   abiToCoreType(a1,a2,a3,a4)=a1:*a2:*a3:*a4:*Nil
--   abiFromCoreType(a1:*a2:*a3:*a4:*Nil)=(a1,a2,a3,a4)
