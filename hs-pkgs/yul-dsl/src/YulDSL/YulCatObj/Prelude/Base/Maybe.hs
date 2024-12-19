{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module YulDSL.YulCatObj.Prelude.Base.Maybe () where

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
  yulNumAdd = (mk_builtin_name @(INTx s n) "add", uncurry (+))
  yulNumMul = (mk_builtin_name @(INTx s n) "mul", uncurry (*))
  yulNumAbs = (mk_builtin_name @(INTx s n) "abs", abs)
  yulNumSig = (mk_builtin_name @(INTx s n) "sig", signum)
  yulNumNeg = (mk_builtin_name @(INTx s n) "neg", negate)

instance ( YulCat eff r ~ m
         , YulO2 r a, YulCatObj (ABITypeDerivedOf a), MaybeYulNum a
         ) => PatternMatchable m (Maybe a) (Maybe (m a)) YulCatObj where
  inCase = \case
    Just a  -> YulFork (YulEmb true) (a >.> YulReduceType)
               >.> YulReduceType
               >.> YulExtendType -- @_ @(NP [BOOL, ABITypeDerivedOf a]) @(Maybe a)
    Nothing -> YulFork (YulEmb false) (YulEmb 0)
               >.> YulReduceType
               >.> YulExtendType

  match mp f = let mp' = mp >.> YulReduceType >.> YulSplit
                   b   = mp' >.> YulExl
                   n   = mp' >.> YulExr
                         >.> YulCoerceType @_ @(NP '[ABITypeDerivedOf a]) @(ABITypeDerivedOf a, NP '[])
                         >.> YulExl @_ @(ABITypeDerivedOf a) @_
                         >.> YulExtendType
                 in ifThenElse b (f (Just n)) (f Nothing)

mk_builtin_name :: forall a. ABITypeable a => String -> String
mk_builtin_name n = "__maybe_" ++ n ++ "_t_" ++ abiTypeCanonName @a
