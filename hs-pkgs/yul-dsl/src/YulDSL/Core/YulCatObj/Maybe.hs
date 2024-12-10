{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module YulDSL.Core.YulCatObj.Maybe where

-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCatObj.ContractABI
import           YulDSL.Core.YulCatObj.YulNum


type MaybeYulNum a = (ABITypeable a, ABITypeable (ABITypeDerivedOf a), YulNum (ABITypeDerivedOf a))

instance MaybeYulNum a => ABITypeable (Maybe a) where
  type instance ABITypeDerivedOf (Maybe a) = (BOOL, ABITypeDerivedOf a)

  abiToCoreType (Just x) = (true, abiToCoreType x)
  abiToCoreType Nothing  = (false, 0)

  abiFromCoreType (b, x) = if b == true then Just (abiFromCoreType x)
                           else Nothing

instance MaybeYulNum a => ABITypeCodec (Maybe a)

instance (MaybeYulNum a, Show a) => YulCatObj (Maybe a)
