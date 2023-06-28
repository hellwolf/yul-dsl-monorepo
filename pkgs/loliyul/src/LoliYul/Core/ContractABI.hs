{-|

Copyright   : (c) Miao ZhiCheng, 2023
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : portable

= Description

The Contract Application Binary Interface (ABI) is the standard way to interact with contracts in the Ethereum
ecosystem, both from outside the blockchain and for contract-to-contract interaction. Its latest specification can be
found at [here](https://docs.soliditylang.org/en/latest/abi-spec.html).

This module provides a type-safe way of interacting with the ABI, and a (WORK-IN-PROGRESS) faithful codec for the
bytestring representation of the data.

-}

module LoliYul.Core.ContractABI
  ( module LoliYul.Core.ContractABI.Types
  ) where

import           LoliYul.Core.ContractABI.Types
