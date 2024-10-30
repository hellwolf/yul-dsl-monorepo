{-|

Module      : Ethereum.ContractABI.ABICoreType
Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : GHC2024

= Description

All derived types and dependent types are mapped to the underlying core types, such that you only need to work with
contract ABI types to support the entire contract ABI specification.

-}

module Ethereum.ContractABI.ABICoreType
  ( ABICoreType (..)
  , WORD, word, wordVal, defWord, maxWord, ABIWordValue (..)
  ) where

-- base
import           Control.Exception (assert)
import           Data.Coerce       (coerce)
import           GHC.TypeNats      (KnownNat, SNat, fromSNat)
--
import           Data.TypeBools    (KnownBool, SBool, toBool)


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
  BOOL' == BOOL'               = True
  (INTx' s n) == (INTx' s' n') = toBool s == toBool s' && fromSNat n == fromSNat n'
  ADDR' == ADDR'               = True
  (BYTESn' n) == (BYTESn' n')  = fromSNat n == fromSNat n'
  BYTES' == BYTES'             = True
  (ARRAY' a) == (ARRAY' b)     = a == b
  _ == _                       = False

-- | Raw storage value for ABI value types.
newtype WORD = WORD Integer deriving newtype (Eq, Ord)

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
