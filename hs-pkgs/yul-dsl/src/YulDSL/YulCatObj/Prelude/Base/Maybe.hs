{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module YulDSL.YulCatObj.Prelude.Base.Maybe where

-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCat
import           YulDSL.Core.YulCatObj


type MaybeYulNum a = ( ABITypeable a
                     , ABITypeable (ABITypeDerivedOf a)
                     , ABITypeCodec (ABITypeDerivedOf a)
                     , Num (ABITypeDerivedOf a)
                     )

instance MaybeYulNum a => ABITypeable (Maybe a) where
  type instance ABITypeDerivedOf (Maybe a) = NP [BOOL, ABITypeDerivedOf a]

  abiToCoreType (Just x) = true :* abiToCoreType x :* Nil
  abiToCoreType Nothing  = false :* 0 :* Nil

  abiFromCoreType (b :* x :* Nil) = case b of
    BOOL True  -> Just (abiFromCoreType x)
    BOOL False -> Nothing

instance MaybeYulNum a => ABITypeCodec (Maybe a)

instance (MaybeYulNum a, Show a) => YulCatObj (Maybe a)

instance ( YulO1 a, YulCatObj (ABITypeDerivedOf a), MaybeYulNum a
         ) => PatternMatchable Maybe a where
  match mfa f = let mfa' = mfa >.> YulReduceType >.> YulSplit
                    b = mfa' >.> YulExl
                    n = mfa' >.> YulExr
                        >.> YulCoerceType @_ @(NP '[ABITypeDerivedOf a]) @(ABITypeDerivedOf a, NP '[])
                        >.> YulExl @_ @(ABITypeDerivedOf a) @_
                        >.> YulExtendType
                in ifThenElse b (f (Just n)) (f Nothing)
