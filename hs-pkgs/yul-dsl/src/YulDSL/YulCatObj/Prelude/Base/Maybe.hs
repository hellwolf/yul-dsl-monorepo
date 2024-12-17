{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module YulDSL.YulCatObj.Prelude.Base.Maybe where

-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCat
import           YulDSL.Core.YulCatObj
import           YulDSL.Core.YulNum


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

instance ValidINTx s n => YulNum (Maybe (INTx s n)) where
  yulNumAdd = ("__maybe_add_t", uncurry (+))
  yulNumMul = ("__maybe_mul_t", uncurry (*))
  yulNumAbs = ("__maybe_abs_t", abs)
  yulNumSig = ("__maybe_sig_t", signum)
  yulNumNeg = ("__maybe_neg_t", negate)

instance ( YulO1 a, YulCatObj (ABITypeDerivedOf a), MaybeYulNum a
         ) => PatternMatchable Maybe a where
  pat = \case
    Just a  -> YulFork (YulEmb true) (a >.> YulReduceType)
               >.> YulReduceType
               >.> YulExtendType -- @_ @(NP [BOOL, ABITypeDerivedOf a]) @(Maybe a)
    Nothing -> YulFork (YulEmb false) (YulEmb 0)
               >.> YulReduceType
               >.> YulExtendType

  match mfa f = let mfa' = mfa >.> YulReduceType >.> YulSplit
                    b = mfa' >.> YulExl
                    n = mfa' >.> YulExr
                        >.> YulCoerceType @_ @(NP '[ABITypeDerivedOf a]) @(ABITypeDerivedOf a, NP '[])
                        >.> YulExl @_ @(ABITypeDerivedOf a) @_
                        >.> YulExtendType
                in ifThenElse b (f (Just n)) (f Nothing)
