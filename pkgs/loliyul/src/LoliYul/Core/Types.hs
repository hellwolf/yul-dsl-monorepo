{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DerivingStrategies #-}
module LoliYul.Core.Types
  ( BOOL, true, false
  , ADDR, to_addr, zero_address
  , SVALUE, def_sval
  , INTx, to_intx
  , UINT256, to_uint256, max_uint256
  , INT256, to_int256
  , ABIValue (..)
  , BYTES (..)
  , (:>)(..), Nil, One
  ) where

import           Data.ByteString (ByteString)
import           Numeric.Natural

------------------------------------------------------------------------------------------------------------------------
-- Primitive (Solidity) ABI Types
------------------------------------------------------------------------------------------------------------------------

-- | ABI address value type.
newtype ADDR = ADDR Integer deriving newtype (Ord, Eq)

to_addr :: Integer -> ADDR
to_addr = ADDR -- FIXME range check

-- | Proverbial zero address.
zero_address :: ADDR
zero_address = ADDR 0

-- | ABI boolean value type.
newtype BOOL = BOOL Bool deriving newtype (Eq)

true :: BOOL
true = BOOL True
false :: BOOL
false = BOOL False

-- | ABI integer value types, where ~s~ is for signess and ~n~ is the multiple of 8 bits
newtype INTx (s :: Bool) (n :: Natural) = INT (Maybe Integer) deriving newtype (Ord, Eq)

-- FIXME range check
to_intx :: Integer -> INTx s n; to_intx = INT . Just
type UINT256 = INTx False 32; to_uint256 :: Integer -> UINT256; to_uint256 = to_intx
type INT256  = INTx  True 32; to_int256  :: Integer ->  INT256; to_int256  = to_intx

max_uint256 :: UINT256
max_uint256 = to_uint256 (2 ^ (256 :: Int))

-- | Native value type for the storage.
type SVALUE = UINT256

-- | ABI bytes reference type.
newtype BYTES = BYTES ByteString

-- | Default storage value.
def_sval :: SVALUE
def_sval = INT (Just 0)

-- ABI value types

-- | ABI value type class.
class ABIValue a where
  from_svalue :: SVALUE -> a
  to_svalue :: a -> SVALUE

instance ABIValue ADDR where
  from_svalue (INT (Just a)) = ADDR a
  from_svalue (INT Nothing)  = zero_address

  to_svalue (ADDR 0) = INT Nothing -- Note: this is arguable but perhaps harmless.
  to_svalue (ADDR a) = INT (Just a)

instance ABIValue BOOL where
  from_svalue (INT (Just 0)) = BOOL False
  from_svalue (INT (Just _)) = BOOL True
  from_svalue (INT _)        = BOOL False
  to_svalue (BOOL False) = INT (Just 0)
  to_svalue (BOOL True)  = INT (Just 1)

instance ABIValue (INTx s n) where
  from_svalue (INT (Just a)) = fromIntegral a
  from_svalue (INT Nothing)  = fromIntegral (0 :: Integer)
  to_svalue = INT . Just . toInteger

-- Num instance for INTx types.

instance Enum (INTx s n) where
  fromEnum (INT (Just a)) = fromEnum a
  fromEnum (INT Nothing)  = 0
  toEnum = INT . Just . toEnum

-- FIXME do range checks
instance Num (INTx s n) where
  (INT (Just a)) + (INT (Just b)) = INT (Just (a + b))
  _ + _                           = INT Nothing
  (INT (Just a)) * (INT (Just b)) = INT (Just (a * b))
  _ * _                           = INT Nothing
  negate (INT (Just a)) = INT (Just (negate a))
  negate _              = INT Nothing
  abs (INT (Just a)) = INT (Just (abs a))
  abs _              = INT Nothing
  signum (INT (Just a)) = INT (Just (signum a))
  signum _              = INT Nothing
  fromInteger a = INT (Just (fromInteger a))

instance Real (INTx s n) where
  toRational (INT (Just a)) = toRational a
  toRational (INT Nothing)  = toRational (0 :: Integer)

instance Integral (INTx s n) where
  toInteger (INT (Just a)) = toInteger a
  toInteger (INT Nothing)  = toInteger (0 :: Integer)
  quotRem (INT (Just a)) (INT (Just b)) = let (c, d) = quotRem a b in
    (INT (Just c), INT (Just d))
  quotRem _ _ = (INT Nothing, INT Nothing)

-- Show instances

deriving instance Show BOOL
deriving instance Show ADDR
deriving instance Show (INTx s n)
deriving instance Show BYTES

------------------------------------------------------------------------------------------------------------------------
-- Heterogeneous List
------------------------------------------------------------------------------------------------------------------------

-- | Type constructor (:>) and data constructor pun for creating heterogeneous list.
data a :> b = a :> b
-- | Operator (:>) being right associative allows bracket-free syntax.
infixr :>

-- | Zero-element type list.
type Nil = () :> ()
-- | Single-element type list.
type One a = a :> ()

instance {-# OVERLAPPABLE #-} (Show a, Show b) => Show (a :> b) where
  show (a :> b) = "(" <> show a <> "," <> show b <> ")"
instance {-# OVERLAPPING  #-} Show a => Show (a :> ()) where
  show (a :> ()) = show a
