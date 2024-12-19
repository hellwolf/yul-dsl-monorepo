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
  yulNumMul :: BuiltInYulFunction (a, a) a
  yulNumAbs :: BuiltInYulFunction a a
  yulNumSig :: BuiltInYulFunction a a
  yulNumNeg :: BuiltInYulFunction a a

class YulNum a => YulNumCmp a where
  yulNumCmp :: (Bool, Bool, Bool) -> BuiltInYulFunction (a, a) BOOL

instance ValidINTx s n => YulNum (INTx s n) where
  yulNumAdd = (mk_builtin_name @(INTx s n) "add", uncurry (+))
  yulNumMul = (mk_builtin_name @(INTx s n) "mul", uncurry (*))
  yulNumAbs = (mk_builtin_name @(INTx s n) "abs", abs)
  yulNumSig = (mk_builtin_name @(INTx s n) "sig", signum)
  yulNumNeg = (mk_builtin_name @(INTx s n) "neg", negate)

instance ValidINTx s n => YulNumCmp (INTx s n) where
  yulNumCmp (True , False, False) = ("lt", BOOL . uncurry ( <))
  yulNumCmp (True , True , False) = ("le", BOOL . uncurry (<=))
  yulNumCmp (False, True , False) = ("eq", BOOL . uncurry (==))
  yulNumCmp (False, True , True ) = ("ge", BOOL . uncurry (>=))
  yulNumCmp (False, False, True ) = ("gt", BOOL . uncurry ( >))
  yulNumCmp _                     = error "yulNumCmp: invalid boolean-switches combo"

--
-- Internal function
--

mk_builtin_name :: forall a. ABITypeable a => String -> String
mk_builtin_name n = "__checked_" ++ n ++ "_t_" ++ abiTypeCanonName @a
