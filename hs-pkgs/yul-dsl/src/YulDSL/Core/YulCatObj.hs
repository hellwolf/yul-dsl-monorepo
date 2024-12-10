module YulDSL.Core.YulCatObj
  (  YulCatObj (yul_prod_objs), YulO1, YulO2, YulO3, YulO4, YulO5
  , YulNum
  ) where

-- constraints
import           Data.Constraint      (Dict (Dict))
-- eth-abi
import           Ethereum.ContractABI


--
-- Objects in the yul category
--

-- | All objects in the yul category is simply a 'YulCatObj'.
class (ABITypeable a, ABITypeCodec a, Show a) => YulCatObj a where
  -- | Possible breakdown of the product object type.
  yul_prod_objs :: forall b c. a ~ (b, c) => Dict (YulCatObj b, YulCatObj c)
  yul_prod_objs = error "yul_prod_objs should only be implemented by the product of YulCatObj"

-- Enumerate the objects for both core and extended ABI types

instance YulCatObj ()
instance YulCatObj ADDR
instance YulCatObj BOOL
instance (KnownBool s, ValidINTn n) => YulCatObj (INTx s n)
instance YulCatObj (NP '[])
instance (YulCatObj x, YulCatObj (NP xs)) => YulCatObj (NP (x:xs))
instance (YulCatObj a1, YulCatObj a2) => YulCatObj (a1, a2) where yul_prod_objs = Dict

-- Convenient aliases for declaring yul objects

type YulO1 a = YulCatObj a
type YulO2 a b = (YulCatObj a, YulCatObj b)
type YulO3 a b c = (YulCatObj a, YulCatObj b, YulCatObj c)
type YulO4 a b c d = (YulCatObj a, YulCatObj b, YulCatObj c, YulCatObj d)
type YulO5 a b c d e = (YulCatObj a, YulCatObj b, YulCatObj c, YulCatObj d, YulCatObj e)

--
-- Number types
--

-- | Number-type objects in the category.
class (Num a, YulCatObj a) => YulNum a

-- | Integer types.
instance (KnownBool s, ValidINTn n) => YulNum (INTx s n)
