{-# OPTIONS_GHC -Wno-orphans #-}
module YulDSL.YulCatObj.Prelude.Base.Num where

--
import YulDSL.Core.YulCat
import YulDSL.Core.YulCatObj
import YulDSL.Core.YulNum

instance (YulO2 a r, YulNum a) => Num (YulCat eff r a) where
  a + b = YulJmpB (yulNumAdd @a) <.< YulProd a b <.< YulDup
  a - b = YulJmpB (yulNumSub @a) <.< YulProd a b <.< YulDup
  a * b = YulJmpB (yulNumMul @a)  <.< YulProd a b <.< YulDup
  abs = YulComp (YulJmpB (yulNumAbs @a))
  signum = YulComp (YulJmpB (yulNumSig @a))
  fromInteger = YulEmb . fromInteger
