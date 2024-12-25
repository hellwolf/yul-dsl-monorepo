{-# OPTIONS_GHC -Wno-orphans #-}
module YulDSL.YulCatObj.Prelude.Base.Num where

--
import           YulDSL.Core.YulCat
import           YulDSL.Core.YulCatObj
import           YulDSL.Core.YulNum

instance (YulO2 a r, YulNum a) => Num (YulCat eff r a) where
  a + b = yulJmpBuiltIn (yulNumAdd @a) <.< YulProd a b <.< YulDup
  a - b = yulJmpBuiltIn (yulNumSub @a) <.< YulProd a b <.< YulDup
  a * b = yulJmpBuiltIn (yulNumMul @a)  <.< YulProd a b <.< YulDup
  abs = YulComp (yulJmpBuiltIn (yulNumAbs @a))
  signum = YulComp (yulJmpBuiltIn (yulNumSig @a))
  fromInteger = YulEmb . fromInteger
