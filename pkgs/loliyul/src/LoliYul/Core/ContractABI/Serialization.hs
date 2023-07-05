{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

This module provides a faithful codec for the /Contract ABI/ types.

!! FIXME !!: Though currently it uses cereal serialization instead.

-}

module LoliYul.Core.ContractABI.Serialization where

import           Data.ByteString                   (ByteString)
import qualified Data.Serialize                    as S
import           GHC.Generics

import           LoliYul.Core.ContractABI.Internal
import           LoliYul.Core.ContractABI.Types

-- | ABI serialization class. FIXME: currently an alias to Serialize from cereal library.
type ABISerialize = S.Serialize

deriving instance Generic ADDR
deriving newtype instance S.Serialize ADDR

deriving instance Generic BOOL
deriving newtype instance S.Serialize BOOL

deriving instance Generic (INTx s n)
deriving newtype instance S.Serialize (INTx s n)

deriving instance Generic BYTES
deriving newtype instance S.Serialize BYTES

deriving instance Generic (a :> b)
deriving anyclass instance (ABISerialize a, ABISerialize b) => S.Serialize (a :> b)

-- | ABI encoder.
abi_encode :: ABISerialize a => a -> ByteString
abi_encode = S.encode

-- | ABI decoder.
abi_decode :: ABISerialize a => ByteString -> Maybe a
abi_decode a = case S.decode a of
                 Right b -> Just b
                 Left _  -> Nothing
