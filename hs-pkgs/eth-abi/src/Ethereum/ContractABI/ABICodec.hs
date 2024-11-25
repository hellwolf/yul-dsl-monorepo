module Ethereum.ContractABI.ABICodec
  ( ABITypeCodec (abiEncoder, abiDecoder)
  , abiEncode, abiDecode
  ) where

-- cereal
import qualified Data.Serialize                   as S
-- bytestring
import qualified Data.ByteString                  as B
--
import           Ethereum.ContractABI.ABITypeable (ABITypeable)


-- | ABI type bytstream codec
class ABITypeable a => ABITypeCodec a where
  abiDecoder :: forall. S.Get a

  abiEncoder :: forall. S.Putter a

  abiEncode :: forall. a -> B.ByteString
  abiEncode = S.runPut . abiEncoder

  abiDecode :: forall. B.ByteString -> Maybe a
  abiDecode = either (const Nothing) Just . S.runGet abiDecoder
