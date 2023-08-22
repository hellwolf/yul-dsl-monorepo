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

module YulDSL.Core.ContractABI
  ( module YulDSL.Core.ContractABI.Types
  , module YulDSL.Core.ContractABI.Serialization
  , module YulDSL.Core.ContractABI.ABIType
  , module YulDSL.Core.ContractABI.Coerce
  ) where

import           YulDSL.Core.ContractABI.ABIType
import           YulDSL.Core.ContractABI.Coerce
import           YulDSL.Core.ContractABI.Serialization
import           YulDSL.Core.ContractABI.Types
