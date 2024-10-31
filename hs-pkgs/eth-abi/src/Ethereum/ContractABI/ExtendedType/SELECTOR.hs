{-# LANGUAGE AllowAmbiguousTypes #-}
module Ethereum.ContractABI.ExtendedType.SELECTOR
 ( FuncSig (FuncSig)
 , SELECTOR (SELECTOR)
 , makeFuncSig
 , mkTypedSelector
 , mkRawSelector
 ) where

-- base
import           Data.Bits                          (shift, (.|.))
import           Data.List                          (intercalate)
import           Data.Maybe                         (fromJust)
import           Data.Word                          (Word32)
import           Numeric                            (showHex)
-- bytesstring
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Char8              as BC
-- crypton
import           Crypto.Hash                        (Digest, Keccak_256, hash)
-- memory
import qualified Data.ByteArray                     as BA
-- eth-abi
import           Ethereum.ContractABI.ABICoreType
import           Ethereum.ContractABI.ABITypeable
import           Ethereum.ContractABI.CoreType.INTx (U32, intxVal)


-- | External function signature. This optional information does not have run-time representation.
newtype FuncSig = FuncSig (String {- function name -}, [String] {- argument types -})

instance Show FuncSig where
  show (FuncSig (fname, args)) = fname ++ "(" ++ intercalate "," args ++ ")"

-- | Selector value type with the optional function signature tagged.
newtype SELECTOR = SELECTOR (U32, Maybe FuncSig)

-- Generate ABI Contract compatible signature string.
makeFuncSig :: forall a. ABITypeable a => String -> FuncSig
makeFuncSig fname = FuncSig (fname, fmap abiCoreTypeCanonName (abiTypeInfo @a))

mkTypedSelector :: forall a. ABITypeable a => String -> SELECTOR
mkTypedSelector fname = SELECTOR (fromJust (fromIntegral sel4bytes), Just (makeFuncSig @a fname))
  where
    sig = show (makeFuncSig @a fname)
    bs4bytes = B.unpack(B.take 4(BA.convert(hash(BC.pack sig) :: Digest Keccak_256) :: B.ByteString))
    sel4bytes = (fromIntegral (bs4bytes!!3) :: Word32)
      .|. shift (fromIntegral (bs4bytes!!2) :: Word32) 8
      .|. shift (fromIntegral (bs4bytes!!1) :: Word32) 16
      .|. shift (fromIntegral (bs4bytes!!0) :: Word32) 24

mkRawSelector :: U32 -> SELECTOR
mkRawSelector sig = SELECTOR (sig, Nothing)

instance Show SELECTOR where
  show (SELECTOR (sig, Just (FuncSig (fname, args)))) = "0x" ++ showHex (intxVal sig) " /* " ++  fname ++ "(" ++ intercalate ","args ++ ") */"
  show (SELECTOR (sig, Nothing))                = "0x" ++ showHex (intxVal sig) ""
