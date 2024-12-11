module YulDSL.Core.YulNum where

-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCatObj

-- | Number-type objects in the category.
class (YulCatObj a, Num a, Ord a) => YulNum a

instance (KnownBool s, ValidINTn n) => YulNum (INTx s n)
