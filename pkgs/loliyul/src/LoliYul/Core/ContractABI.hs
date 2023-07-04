{-# LANGUAGE ExplicitNamespaces #-}
{-|

Copyright   : (c) Miao ZhiCheng, 2023
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

The Contract Application Binary Interface (ABI) is the standard way to interact with contracts in the Ethereum
ecosystem, both from outside the blockchain and for contract-to-contract interaction. Its latest specification can be
found at [here](https://docs.soliditylang.org/en/latest/abi-spec.html).

This module provides a type-safe way of interacting with the ABI, and a (WORK-IN-PROGRESS) faithful codec for the
bytestring representation of the data.

-}

module LoliYul.Core.ContractABI
  ( module LoliYul.Core.ContractABI.Types
  , ABIType (..)
  , module LoliYul.Core.ContractABI.Serialization
  ) where

import           Data.Constraint                        (Dict (..))
import           Data.Typeable                          (Typeable)
import           GHC.TypeNats                           (KnownNat, Nat)

import           LoliYul.Core.ContractABI.Serialization
import           LoliYul.Core.ContractABI.Types

-- | Contract ABI type class for all primitive and composite ABI types.
class (Show a, Typeable a, ABISerialize a) => ABIType a where
  -- | Possible breakdown of the product object type.
  maybe_prod_objs :: forall b c. a ~ (b, c) => Dict (ABIType b, ABIType c)
  maybe_prod_objs = error "maybe_prod_objs: not a product object"

-- Primitive types:

instance ABIType ()
instance ABIType ADDR
instance ABIType BOOL
instance forall (s :: Bool) (n :: Nat) .(Typeable s, KnownNat n) => ABIType (INTx s n)
instance ABIType BYTES

-- Composite types:

instance forall a b. (ABIType a, ABIType b) => ABIType (a , b) where
  maybe_prod_objs = Dict

instance forall a b. (ABIType a, ABIType b) => ABIType (a :> b)
