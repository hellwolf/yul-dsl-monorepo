{-# LANGUAGE AllowAmbiguousTypes #-}
module YulDSL.Core.YulNum
  ( YulNum (..)
  , YulNumCmp (..)
  ) where

-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCatObj

-- | Number-type objects in the category.
class (YulCatObj a, Num a, Ord a) => YulNum a where
  yulNumAdd :: BuiltInYulFunction (a, a) a
  yulNumSub :: BuiltInYulFunction (a, a) a
  yulNumMul :: BuiltInYulFunction (a, a) a
  yulNumAbs :: BuiltInYulFunction a a
  yulNumSig :: BuiltInYulFunction a a

class YulNum a => YulNumCmp a where
  yulNumCmp :: (Bool, Bool, Bool) -> BuiltInYulFunction (a, a) BOOL

instance ValidINTx s n => YulNum (INTx s n) where
  yulNumAdd = (mk_num_op @(INTx s n) "add", uncurry (+))
  yulNumSub = (mk_num_op @(INTx s n) "sub", uncurry (-))
  yulNumMul = (mk_num_op @(INTx s n) "mul", uncurry (*))
  yulNumAbs = (mk_num_op @(INTx s n) "abs", abs)
  yulNumSig = (mk_num_op @(INTx s n) "sig", signum)

instance ValidINTx s n => YulNumCmp (INTx s n) where
  yulNumCmp (True , False, False) = (mk_cmp_op @(INTx s n) "lt", BOOL . uncurry ( <))
  yulNumCmp (True , True , False) = (mk_cmp_op @(INTx s n) "le", BOOL . uncurry (<=))
  yulNumCmp (False, True , False) = (mk_cmp_op @(INTx s n) "eq", BOOL . uncurry (==))
  yulNumCmp (False, True , True ) = (mk_cmp_op @(INTx s n) "ge", BOOL . uncurry (>=))
  yulNumCmp (False, False, True ) = (mk_cmp_op @(INTx s n) "gt", BOOL . uncurry ( >))
  yulNumCmp _                     = error "yulNumCmp: invalid boolean-switches combo"

--
-- Internal function
--

mk_num_op :: forall a. ABITypeable a => String -> String
mk_num_op n = "__checked_" ++ n ++ "_t_" ++ abiTypeCanonName @a

mk_cmp_op :: forall a s n. (a ~ INTx s n, ValidINTx s n) => String -> String
mk_cmp_op op = "__cmp_" ++ (if fromBoolKind @s then 's':op else op)  ++ "_" ++ abiTypeCanonName @a
