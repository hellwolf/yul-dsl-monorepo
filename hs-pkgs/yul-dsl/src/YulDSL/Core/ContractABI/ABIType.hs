{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}

module YulDSL.Core.ContractABI.ABIType where

import           Data.Constraint                       (Dict (..))
import           Data.Typeable                         (Proxy (..), Typeable, typeRep)
import           GHC.TypeNats                          (KnownNat, natVal)

import           YulDSL.Core.ContractABI.Serialization (ABISerialize)
import           YulDSL.Core.ContractABI.Types


-- | Contract ABI type class for all primitive and composite ABI types.
class (Show a, Typeable a, ABISerialize a) => ABIType a where
  -- | Possible breakdown of the product object type.
  maybe_prod_objs :: forall b c. a ~ (b, c) => Dict (ABIType b, ABIType c)
  maybe_prod_objs = error "maybe_prod_objs: not a product object"

  abi_type_name :: String

  abi_type_count_vars :: Int

-- Primitive types:

instance ABIType () where abi_type_name = "∅"; abi_type_count_vars = 0
instance ABIType ADDR where abi_type_name = "ADDR"; abi_type_count_vars = 1
instance ABIType BOOL where abi_type_name = "BOOL"; abi_type_count_vars = 1
instance forall s n. (Typeable s, KnownNat n) => ABIType (INTx s n) where
  abi_type_name = (if typeRep (Proxy :: Proxy s) == typeRep (Proxy :: Proxy True)
                   then "INT" else "UINT") <> show (natVal (Proxy :: Proxy n) * 8)
  abi_type_count_vars = 1
instance ABIType BYTES where abi_type_name = "BYTES"; abi_type_count_vars = 1

-- Composite types:

instance forall a b. (ABIType a, ABIType b) => ABIType (a, b) where
  maybe_prod_objs = Dict
  abi_type_name = "(" <> abi_type_name @a <> "×" <> abi_type_name @b <> ")"
  abi_type_count_vars = abi_type_count_vars @a + abi_type_count_vars @b

instance forall a b. (ABIType a, ABIType b) => ABIType (a :> b) where
  abi_type_name = "(" <> abi_type_name @a <> ":>" <> abi_type_name @b <> ")"
  abi_type_count_vars = abi_type_count_vars @a + abi_type_count_vars @b

instance ABIType a => ABIType [a] where
  abi_type_name = "[" <> abi_type_name @a <> "]"
  abi_type_count_vars = 1

-- | A 'abi_type_name' variant, enclosing name with "@()".
abi_type_name' :: forall a. ABIType a => String
abi_type_name' = "@(" <> abi_type_name @a <> ")"

-- External Call Specifications:
