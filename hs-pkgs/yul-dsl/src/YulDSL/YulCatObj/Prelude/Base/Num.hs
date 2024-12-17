{-# OPTIONS_GHC -Wno-orphans #-}
module YulDSL.YulCatObj.Prelude.Base.Num where

--
import           YulDSL.Core.YulCat
import           YulDSL.Core.YulCatObj
import           YulDSL.Core.YulNum

instance (YulO2 a r, YulNum a) => Num (YulCat eff r a) where
  a + b = jmpBuiltIn (yulNumAdd @a) <.< YulProd a b <.< YulDup
  a * b = jmpBuiltIn (yulNumMul @a)  <.< YulProd a b <.< YulDup
  abs = YulComp (jmpBuiltIn (yulNumAbs @a))
  signum = YulComp (jmpBuiltIn (yulNumSig @a))
  fromInteger = YulEmb . fromInteger
  negate a = jmpBuiltIn (yulNumNeg @a) <.< a
