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
  a + b = YulNumAdd <.< YulProd a b <.< YulDup
  a * b = YulNumMul <.< YulProd a b <.< YulDup
  abs = YulComp YulNumAbs
  signum = YulComp YulNumSig
  fromInteger = YulEmbed . fromInteger
  negate a = YulNumNeg <.< a

instance ValidINTx s n => YulNum (Maybe (INTx s n))
