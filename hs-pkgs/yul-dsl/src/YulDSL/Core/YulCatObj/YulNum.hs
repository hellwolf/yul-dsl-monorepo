module YulDSL.Core.YulCatObj.YulNum where

-- constraints
import           Data.Constraint                   (Dict (Dict))
-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCatObj.ContractABI

--
-- Number types
--

-- | Number-type objects in the category.
class (Num a, YulCatObj a) => YulNum a

-- | Integer types.
instance (KnownBool s, ValidINTn n) => YulNum (INTx s n)
