{-# LANGUAGE TemplateHaskell #-}
{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : hellwolf@yolc.dev
Stability   : experimental
Portability : GHC2024

= Description

All derived types and dependent types are mapped to the underlying core types, such that you only need to work with
contract ABI types to support the entire contract ABI specification.

-}
module Ethereum.ContractABI.ABICoreType
  ( ABICoreType (..)
  -- for working with INTx, BYTEn
  , ValidINTn, Nat, natSing, natVal
  -- ABI type names
  , abiCoreTypeCanonName, abiCoreTypeCompactName
  -- EVM word representations
  , WORD, word, wordVal, defWord, maxWord, ABIWordValue (toWord, fromWord)
  ) where

-- base
import           Control.Exception       (assert)
import           Data.Char               (toUpper)
import           Data.Coerce             (coerce)
import           GHC.TypeLits            (KnownNat (natSing), Nat, SNat, fromSNat, natVal)
import           Numeric                 (showHex)
-- template-haskell
import qualified Language.Haskell.TH     as TH
--
import           Internal.Data.Type.Bool (KnownBool, SBool, toBool)


{- * ABICoreType and their utilities -}

-- | A constraint that restricts what Nat values are valid for 'INTx' and 'BYTESn'.
--   Note: It is valid from 1 to 32.
class KnownNat n => ValidINTn n

-- | A top-level splice that declares all the valid INTx n values.
flip foldMap [1..32] $ \i -> [d| instance ValidINTn $(TH.litT (TH.numTyLit i)) |]

-- | Contract ABI core types.
data ABICoreType where
  -- ^ Boolean
  BOOL'   :: ABICoreType
  -- ^ Fixed-precision integers
  INTx'   :: forall s n. (KnownBool s, KnownNat n) => SBool s -> SNat n -> ABICoreType
  -- ^ Ethereum addresses
  ADDR'   :: ABICoreType
  -- ^ Fixed-size byte arrays
  BYTESn' :: forall n. (KnownNat n) => SNat n -> ABICoreType
  -- ^ Packed byte arrays
  BYTES'  :: ABICoreType
  -- ^ Arrays of values of the same ABI core type
  ARRAY'  :: ABICoreType -> ABICoreType

instance Eq ABICoreType where
  BOOL'       == BOOL'         = True
  (INTx' s n) == (INTx' s' n') = toBool s == toBool s' && fromSNat n == fromSNat n'
  ADDR'       == ADDR'         = True
  (BYTESn' n) == (BYTESn' n')  = fromSNat n == fromSNat n'
  BYTES'      == BYTES'        = True
  (ARRAY' a)  == (ARRAY' b)    = a == b
  -- not using _ == _ in order to let GHC do exhaustive checks on cases above
  BOOL'       == _             = False
  (INTx' _ _) == _             = False
  ADDR'       == _             = False
  (BYTESn' _) == _             = False
  BYTES'      == _             = False
  (ARRAY' _)  == _             = False

-- | Canonical names for the core types used for computing the function selectors.
abiCoreTypeCanonName :: ABICoreType -> String
abiCoreTypeCanonName BOOL'       = "bool"
abiCoreTypeCanonName (INTx' s n) = (if toBool s then "int" else "uint") <> show (natVal n * 8)
abiCoreTypeCanonName ADDR'       = "address"
abiCoreTypeCanonName (BYTESn' n) = "bytes" ++ show (natVal n)
abiCoreTypeCanonName BYTES'      = "bytes"
abiCoreTypeCanonName (ARRAY' a)  = abiCoreTypeCanonName a ++ "[]"

-- | Compact but unambiguous names for the core types..
abiCoreTypeCompactName :: ABICoreType -> String
abiCoreTypeCompactName BOOL'       = "b"
abiCoreTypeCompactName (INTx' s n) = (if toBool s then "i" else "u") <> show (natVal n)
abiCoreTypeCompactName ADDR'       = "a"
abiCoreTypeCompactName (BYTESn' n) = "B" ++ show (natVal n)
abiCoreTypeCompactName BYTES'      = "Bs"
abiCoreTypeCompactName (ARRAY' a)  = "A" ++ abiCoreTypeCompactName a

instance Show ABICoreType where show = abiCoreTypeCanonName

{- * EVM word representations  -}

-- | Raw storage value for ABI value types.
newtype WORD = WORD Integer deriving newtype (Eq)

instance Show WORD where
    show (WORD a) = "0x" ++ fmap toUpper (showHex a "")

word :: Integer -> WORD
word a = assert (a >= 0 && a <= coerce maxWord) WORD a

wordVal :: WORD -> Integer
wordVal = coerce

-- | Default and minimum word value: 0.
defWord :: WORD
defWord = WORD 0

-- | Maximum word value: 2^256 - 1.
maxWord :: WORD
maxWord = WORD (2 ^ (256 :: Int) - 1)

-- | ABI values that can be stored in one word.
class Bounded a => ABIWordValue a where
  -- | Convert from a storage value to an ABI typed value.
  fromWord :: WORD -> Maybe a
  -- | Convert from a ABI typed value to a storage value.
  toWord   :: a -> WORD
