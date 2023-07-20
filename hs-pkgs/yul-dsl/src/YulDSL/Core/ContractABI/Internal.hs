{-# LANGUAGE DerivingStrategies #-}

{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

This module exposes the internal of the ABI types, so that serialization can use it.

-}

module YulDSL.Core.ContractABI.Internal where

import           Data.Word       (Word32)
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

newtype Selector = MkSelector Word32

data CallSpec a b = ExternalCall ADDR Selector
                  | DelegateCall ADDR Selector
                  | StaticCall   ADDR Selector
