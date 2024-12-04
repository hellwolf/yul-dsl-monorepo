module YulDSL.Core.YulCatObj
  (  YulObj (yul_prod_objs), YulO1, YulO2, YulO3, YulO4, YulO5
  , YulNum
  ) where

-- constraints
import           Data.Constraint      (Dict (Dict))
-- eth-abi
import           Ethereum.ContractABI


--
-- Objects in the yul category
--

-- | All objects in the yul category is simply a 'YulObj'.
class (ABITypeable a, ABITypeCodec a, Show a) => YulObj a where
  -- | Possible breakdown of the product object type.
  yul_prod_objs :: forall b c. a ~ (b, c) => Dict (YulObj b, YulObj c)
  yul_prod_objs = error "yul_prod_objs should only be implemented by the product of YulObj"

-- Enumerate the objects for both core and extended ABI types

instance YulObj ()
instance YulObj ADDR
instance YulObj BOOL
instance (KnownBool s, ValidINTn n) => YulObj (INTx s n)
instance YulObj (NP '[])
instance (YulObj x, YulObj (NP xs)) => YulObj (NP (x:xs))
instance (YulObj a1, YulObj a2) => YulObj (a1, a2) where yul_prod_objs = Dict

-- Convenient aliases for declaring yul objects

type YulO1 a = YulObj a
type YulO2 a b = (YulObj a, YulObj b)
type YulO3 a b c = (YulObj a, YulObj b, YulObj c)
type YulO4 a b c d = (YulObj a, YulObj b, YulObj c, YulObj d)
type YulO5 a b c d e = (YulObj a, YulObj b, YulObj c, YulObj d, YulObj e)

--
-- Number types
--

-- | Number-type objects in the category.
class (Num a, YulObj a) => YulNum a

-- | Integer types.
instance (KnownBool s, ValidINTn n) => YulNum (INTx s n)
