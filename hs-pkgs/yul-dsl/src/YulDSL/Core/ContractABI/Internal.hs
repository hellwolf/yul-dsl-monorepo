{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

This module exposes the internal of the ABI types, so that serialization can use it.

-}

module YulDSL.Core.ContractABI.Internal where

import           GHC.Natural     (Natural)
import           GHC.TypeNats    (Nat)

import           Data.ByteString (ByteString)

-- | ABI address value type.
newtype ADDR = ADDR Natural deriving newtype (Ord, Eq)

-- | ABI boolean value type.
newtype BOOL = BOOL Bool deriving newtype (Eq)

-- | ABI integer value types, where @s@ is for signess and @n@ is the multiple of 8 bits
newtype INTx (s :: Bool) (n :: Nat) = INT (Maybe Integer) deriving newtype (Ord, Eq)

-- | ABI bytes reference type.
newtype BYTES = BYTES ByteString deriving newtype (Eq)


-- External Call Specification:

-- | External function signature.
type FuncSig = Maybe String

-- | Selector value type.
type Sel4Bytes = INTx False 4

-- | Selector value type with the optional function signature tagged.
newtype SEL = SEL (FuncSig, Sel4Bytes)

-- | Storage location for the external function call.
data FuncStorage = FuncExternal | FuncDelegated

-- | Effect type for the external function call.
data FuncEffect = FuncTx | FuncStatic

-- | External function specification.
newtype FUNC a b = FUNC (FuncStorage, FuncEffect, SEL, ADDR)
