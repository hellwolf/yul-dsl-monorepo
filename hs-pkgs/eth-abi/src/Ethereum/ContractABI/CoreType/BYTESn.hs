{-# LANGUAGE TemplateHaskell #-}
module Ethereum.ContractABI.CoreType.BYTESn
  ( BYTESn
  , B1, B2, B3, B4, B5, B6, B7, B8
  , B9, B10, B11, B12, B13, B14, B15, B16
  , B17, B18, B19, B20, B21, B22, B23, B24
  , B25, B26, B27, B28, B29, B30, B31, B32
  ) where

-- base
import           Control.Monad                      (forM)
-- template-haskell
import qualified Language.Haskell.TH                as TH
--
import           Ethereum.ContractABI.ABICoreType
import           Ethereum.ContractABI.ABITypeable
import           Ethereum.ContractABI.ABITypeCodec
import           Ethereum.ContractABI.CoreType.INTx

-- | BYTESn is a new type of INTx, which has its own name in function selectors.
newtype BYTESn n = BYTESn (INTx False n)
  deriving newtype (Eq, Ord, Enum)

instance forall n. (ValidINTn n) => ABITypeable (BYTESn n) where
  type instance ABITypeDerivedOf (BYTESn n) = BYTESn n
  abiTypeInfo = [BYTESn' (natSing @n)]

deriving newtype instance forall n. (ValidINTn n) => ABITypeCodec (BYTESn n)

deriving newtype instance forall n. (ValidINTn n) => Show (BYTESn n)

{- * Assorted Fixed-Precision Integer Aliases -}

forM [1..32] $ \n -> do
  name <- TH.newName ("B" ++ show n)
  TH.tySynD name [] ((TH.conT ''BYTESn)
                      `TH.appT` (TH.litT (TH.numTyLit n)))
