{-# OPTIONS_GHC -Wno-orphans #-}
module YulDSL.YulCatObj.Prelude.Base.Num where

-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCat
import           YulDSL.Core.YulCatObj
import           YulDSL.Core.YulNum
import           YulDSL.YulCatObj.Prelude.Base.Maybe ()

instance (YulO2 a r, YulNum a) => Num (YulCat eff r a) where
  a + b = jmpBuiltIn (yulNumAdd @a) <.< YulProd a b <.< YulDup
  a * b = jmpBuiltIn (yulNumMul @a)  <.< YulProd a b <.< YulDup
  abs = YulComp (jmpBuiltIn (yulNumAbs @a))
  signum = YulComp (jmpBuiltIn (yulNumSig @a))
  fromInteger = YulEmb . fromInteger
  negate a = jmpBuiltIn (yulNumNeg @a) <.< a

instance ValidINTx s n => YulNum (Maybe (INTx s n)) where
  yulNumCmp (True , False, False) = ("lt(_)", BOOL . uncurry (<))
  yulNumCmp (True , True , False) = ("iszero(gt(_))", BOOL . uncurry (<=))
  yulNumCmp (False, True , False) = ("eq(_)", BOOL . uncurry (==))
  yulNumCmp (False, True , True ) = ("iszero(lt(_))", BOOL . uncurry (>=))
  yulNumCmp (False, False, True ) = ("gt(_)", BOOL . uncurry (>))
  yulNumCmp  _                    = error "yulNumCmp: invalid boolean-switches combo"

  yulNumAdd = ("__maybe_add_t", uncurry (+))

  yulNumMul = ("__maybe_mul_t", uncurry (*))

  yulNumAbs = ("__maybe_abs_t", abs)

  yulNumSig = ("__maybe_sig_t", signum)

  yulNumNeg = ("__maybe_neg_t", negate)
