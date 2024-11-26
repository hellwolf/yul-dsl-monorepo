{-# OPTIONS_GHC -Wno-orphans #-}

module Ethereum.ContractABI.ExtendedType.TUPLEn where

-- ghc-experimental
-- import           Data.Tuple.Experimental
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

type family TupleNtoNP t where
  TupleNtoNP ()  = NP '[]
  TupleNtoNP (x1, x2) = NP '[x1, x2]
  TupleNtoNP (x1, x2, x3) = NP '[x1, x2, x3]
  TupleNtoNP (x1, x2, x3, x4) = NP '[x1, x2, x3, x4]
  TupleNtoNP (x1) = NP '[x1] -- keep the solo tuple in the end to avoid overlapping type instances

type family NPtoTupleN np where
  NPtoTupleN (NP '[]) = ()
  NPtoTupleN (NP '[x1]) = x1
  NPtoTupleN (NP '[x1,x2]) = (x1,x2)
  NPtoTupleN (NP '[x1,x2,x3]) = (x1,x2,x3)
  NPtoTupleN (NP '[x1,x2,x3,x4]) = (x1,x2,x3,x4)

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
