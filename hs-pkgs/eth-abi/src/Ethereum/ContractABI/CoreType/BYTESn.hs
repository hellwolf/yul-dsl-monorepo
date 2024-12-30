{-# LANGUAGE AllowAmbiguousTypes #-}
module Ethereum.ContractABI.CoreType.BYTESn
  ( BYTESn (unBYTESn)
  , bytesnNBytes, bytesnToInteger, bytesnFromWord8s
  , stringKeccak256
  , B1, B2, B3, B4, B5, B6, B7, B8
  , B9, B10, B11, B12, B13, B14, B15, B16
  , B17, B18, B19, B20, B21, B22, B23, B24
  , B25, B26, B27, B28, B29, B30, B31, B32
  ) where

-- base
import Control.Exception                 (assert)
import Data.Word                         (Word8)
import Numeric                           (showHex)
-- bytestring
import Data.ByteString                   qualified as BS
import Data.ByteString.Char8 qualified
-- memory
import Data.ByteArray                    qualified as BA
-- crypton
import Crypto.Hash                       qualified as Hash
-- cereal
import Data.Serialize                    qualified as S
--
import Ethereum.ContractABI.ABICoreType
import Ethereum.ContractABI.ABITypeable
import Ethereum.ContractABI.ABITypeCodec


-- | BYTESn is a new type of list of 'Word8' with number of bytes tagged, and with least-significant byte first.
newtype BYTESn n = BYTESn { unBYTESn :: [Word8] } deriving (Eq, Ord)

bytesnNBytes :: forall n. ValidINTn n => Int
bytesnNBytes = fromInteger . fromSNat $ natSing @n

bytesnToInteger :: forall n. ValidINTn n => BYTESn n -> Integer
bytesnToInteger bn =
  foldl' (\b (i, w) -> b + toInteger w * (2 ^ i)) 0 (zip [0 .. bytesnNBytes @n] (unBYTESn bn))

-- | Create BYTESn from a list of 'Word8'.
bytesnFromWord8s :: forall n. ValidINTn n => [Word8] -> BYTESn n
bytesnFromWord8s ws = assert (toInteger (length ws) == fromSNat (natSing @n)) (BYTESn ws)

-- | Keccack256 of a string value.
stringKeccak256:: String -> BYTESn 32
stringKeccak256 s = let hash = Hash.hash (Data.ByteString.Char8.pack s) :: Hash.Digest Hash.Keccak_256
                    in BYTESn $ BS.unpack (BA.convert hash :: BS.ByteString)


--
-- Instances
--

instance (ValidINTn n) => ABITypeable (BYTESn n) where
  type instance ABITypeDerivedOf (BYTESn n) = BYTESn n
  type instance ABITypeValueSize (BYTESn n) = n
  abiTypeInfo = [BYTESn' (natSing @n)]

instance (ValidINTn n) => ABITypeCodec (BYTESn n) where
  abiDecoder = fmap BYTESn S.get
  abiEncoder = S.put . unBYTESn

instance Show (BYTESn n) where
  show (BYTESn ws) = "0x" ++ concatMap show_word8 ws

instance ValidINTn n => Bounded (BYTESn n) where
  minBound = BYTESn $ replicate (bytesnNBytes @n) minBound
  maxBound = BYTESn $ replicate (bytesnNBytes @n) maxBound

instance ValidINTn n => ABIWordValue (BYTESn n) where
  fromWord w = let v = wordToInteger w
                   (v', ws) = foldl'
                                (\(v'', ws') _ -> (v'' `div` 256, fromInteger (v'' `rem` 256) : ws'))
                                (v, [])
                                [0 .. bytesnNBytes @n]
               in if v' == 0 then Just (BYTESn ws) else Nothing
  toWord = integerToWord . bytesnToInteger

--
-- Internal function
--

show_word8 :: Word8 -> String
show_word8 w = let h = showHex (toInteger w) ""
                   pad2 [c] = ['0', c]
                   pad2 xs  = xs
               in pad2 h

--
-- Assorted Fixed-Precision Integer Aliases
--

-- shell: $ for i in `seq 1 32`;do echo "type B$i = BYTESn $i";done

type B1 = BYTESn 1
type B2 = BYTESn 2
type B3 = BYTESn 3
type B4 = BYTESn 4
type B5 = BYTESn 5
type B6 = BYTESn 6
type B7 = BYTESn 7
type B8 = BYTESn 8
type B9 = BYTESn 9
type B10 = BYTESn 10
type B11 = BYTESn 11
type B12 = BYTESn 12
type B13 = BYTESn 13
type B14 = BYTESn 14
type B15 = BYTESn 15
type B16 = BYTESn 16
type B17 = BYTESn 17
type B18 = BYTESn 18
type B19 = BYTESn 19
type B20 = BYTESn 20
type B21 = BYTESn 21
type B22 = BYTESn 22
type B23 = BYTESn 23
type B24 = BYTESn 24
type B25 = BYTESn 25
type B26 = BYTESn 26
type B27 = BYTESn 27
type B28 = BYTESn 28
type B29 = BYTESn 29
type B30 = BYTESn 30
type B31 = BYTESn 31
type B32 = BYTESn 32

-- forM [1..32] $ \n -> do
--   name <- TH.newName ("B" ++ show n)
--   TH.tySynD name [] ((TH.conT ''BYTESn)
--                       `TH.appT` (TH.litT (TH.numTyLit n)))
