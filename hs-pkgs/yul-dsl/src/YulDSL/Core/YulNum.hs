{-# LANGUAGE AllowAmbiguousTypes #-}
module YulDSL.Core.YulNum where

-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCatObj

-- | Number-type objects in the category.
class (YulCatObj a, Num a, Ord a) => YulNum a where
  yulNumCmp :: (Bool, Bool, Bool) -> BuiltInYulFunction (a, a) BOOL
  yulNumAdd :: BuiltInYulFunction (a, a) a
  yulNumMul :: BuiltInYulFunction (a, a) a
  yulNumAbs :: BuiltInYulFunction a a
  yulNumSig :: BuiltInYulFunction a a
  yulNumNeg :: BuiltInYulFunction a a

instance ValidINTx s n => YulNum (INTx s n) where
  yulNumCmp (True , False, False) = ("lt", BOOL . uncurry (<))
  yulNumCmp (True , True , False) = ("le", BOOL . uncurry (<=))
  yulNumCmp (False, True , False) = ("eq", BOOL . uncurry (==))
  yulNumCmp (False, True , True ) = ("ge", BOOL . uncurry (>=))
  yulNumCmp (False, False, True ) = ("gt", BOOL . uncurry (>))
  yulNumCmp  _                    = error "yulNumCmp: invalid boolean-switches combo"

  yulNumAdd = ("__checked_add_t", uncurry (+))

  yulNumMul = ("__checked_mul_t", uncurry (*))

  yulNumAbs = ("__checked_abs_t", abs)

  yulNumSig = ("__checked_sig_t", signum)

  yulNumNeg = ("__checked_neg_t", negate)
