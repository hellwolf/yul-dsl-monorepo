{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DerivingStrategies  #-}

module LoliYul.Core.ContractABI.Types
  ( SVALUE, def_sval, max_sval
  , BOOL, true, false, if'
  , ADDR, to_addr, to_addr', zero_address
  , INTx, intx_sign, intx_nbits, min_intx, max_intx, to_intx
  , UINT8,UINT16,UINT24,UINT32,UINT40,UINT48,UINT56,UINT64
  , UINT72,UINT80,UINT88,UINT96,UINT104,UINT112,UINT120,UINT128
  , UINT136,UINT144,UINT152,UINT160,UINT168,UINT176,UINT184,UINT192
  , UINT200,UINT208,UINT216,UINT224,UINT232,UINT240,UINT248,UINT256
  , INT8,INT16,INT24,INT32,INT40,INT48,INT56,INT64
  , INT72,INT80,INT88,INT96,INT104,INT112,INT120,INT128
  , INT136,INT144,INT152,INT160,INT168,INT176,INT184,INT192
  , INT200,INT208,INT216,INT224,INT232,INT240,INT248,INT256
  , ABIValue(..)
  , BYTES(..)
  , (:>)(..), Nil, One
  ) where

import           Data.Bits       (shift)
import           Data.ByteString (ByteString)
import           Data.Maybe      (fromJust)
import           Data.Typeable   (Proxy (..), Typeable, typeRep)
import           GHC.Natural     (Natural, naturalFromInteger)
import           GHC.TypeNats    (KnownNat, Nat, natVal)

------------------------------------------------------------------------------------------------------------------------
-- Primitive (Solidity) ABI Types
------------------------------------------------------------------------------------------------------------------------

-- | Storage value.
newtype SVALUE = SVALUE Natural deriving newtype (Eq, Show)

-- | Default storage value.
def_sval :: SVALUE
def_sval = SVALUE 0

-- | Maximum storage value.
max_sval :: SVALUE
max_sval = SVALUE (2 ^ (256 :: Natural) - 1)

-- | ABI address value type.
newtype ADDR = ADDR Natural deriving newtype (Ord, Eq)

-- | From integer to ADDR.
to_addr :: Natural -> Maybe ADDR
to_addr = Just . ADDR -- FIXME range check

to_addr' :: Natural -> ADDR
to_addr' = fromJust . to_addr

-- | The proverbial zero address.
zero_address :: ADDR
zero_address = ADDR 0

-- | ABI boolean value type.
newtype BOOL = BOOL Bool deriving newtype (Eq)

-- | True value for 'BOOL'.
true :: BOOL
true = BOOL True

-- | False value for 'BOOL'.
false :: BOOL
false = BOOL False

-- | Function of if-then-else for 'BOOL'.
if' :: BOOL -> a -> a -> a
if' (BOOL True) x _  = x
if' (BOOL False) _ y = y

-- | ABI integer value types, where ~s~ is for signess and ~n~ is the multiple of 8 bits
newtype INTx (s :: Bool) (n :: Nat) = INT (Maybe Integer) deriving newtype (Ord, Eq)

-- | Number of bits for the INTx type. Use type application on ~a~.
intx_nbits :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, KnownNat n) => Int
intx_nbits = fromEnum (8 * natVal (Proxy @n))

-- | Sign of the INTx type. Use type application on ~a~.
intx_sign :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, Typeable s, KnownNat n) => Bool
intx_sign = typeRep (Proxy @s) == typeRep (Proxy @True)

-- | Minimum value of the INTx type. Use type application on ~a~.
min_intx :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, Typeable s, KnownNat n) => a
min_intx = INT . Just $
  if intx_sign @(INTx s n) then negate (1 `shift` (nbits - 1)) else 0
  where nbits = intx_nbits @(INTx s n)

-- | Maximum value of the INTx type. Use type application on ~a~.
max_intx :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, Typeable s, KnownNat n) => a
max_intx = INT . Just $
  if intx_sign @(INTx s n) then (1 `shift` (nbits - 1)) - 1 else (1 `shift` nbits) - 1
  where nbits = intx_nbits @(INTx s n)

instance (Typeable s, KnownNat n) => Bounded (INTx s n) where
  minBound = min_intx
  maxBound = max_intx

to_intx :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, Typeable s, KnownNat n) => Integer -> a
to_intx = fromInteger

-- = Assorted integer types
--
-- == Code generation command
--   sh$ for i in `seq 1 32`;do echo "type UINT$((i*8)) = INTx False $i;type INT$((i*8)) = INTx True $i";done
--
-- == Examples
-- >>> (min_intx @INT32, max_intx @INT32)
-- (INT (Just (-2147483648)),INT (Just 2147483647))
-- >>> (min_intx @UINT32, max_intx @UINT32)
-- (INT (Just 0),INT (Just 4294967295))
-- >>> (min_intx @INT96, max_intx @INT96)
-- (INT (Just (-39614081257132168796771975168)),INT (Just 39614081257132168796771975167))
type UINT8 = INTx False 1;type INT8 = INTx True 1
type UINT16 = INTx False 2;type INT16 = INTx True 2
type UINT24 = INTx False 3;type INT24 = INTx True 3
type UINT32 = INTx False 4;type INT32 = INTx True 4
type UINT40 = INTx False 5;type INT40 = INTx True 5
type UINT48 = INTx False 6;type INT48 = INTx True 6
type UINT56 = INTx False 7;type INT56 = INTx True 7
type UINT64 = INTx False 8;type INT64 = INTx True 8
type UINT72 = INTx False 9;type INT72 = INTx True 9
type UINT80 = INTx False 10;type INT80 = INTx True 10
type UINT88 = INTx False 11;type INT88 = INTx True 11
type UINT96 = INTx False 12;type INT96 = INTx True 12
type UINT104 = INTx False 13;type INT104 = INTx True 13
type UINT112 = INTx False 14;type INT112 = INTx True 14
type UINT120 = INTx False 15;type INT120 = INTx True 15
type UINT128 = INTx False 16;type INT128 = INTx True 16
type UINT136 = INTx False 17;type INT136 = INTx True 17
type UINT144 = INTx False 18;type INT144 = INTx True 18
type UINT152 = INTx False 19;type INT152 = INTx True 19
type UINT160 = INTx False 20;type INT160 = INTx True 20
type UINT168 = INTx False 21;type INT168 = INTx True 21
type UINT176 = INTx False 22;type INT176 = INTx True 22
type UINT184 = INTx False 23;type INT184 = INTx True 23
type UINT192 = INTx False 24;type INT192 = INTx True 24
type UINT200 = INTx False 25;type INT200 = INTx True 25
type UINT208 = INTx False 26;type INT208 = INTx True 26
type UINT216 = INTx False 27;type INT216 = INTx True 27
type UINT224 = INTx False 28;type INT224 = INTx True 28
type UINT232 = INTx False 29;type INT232 = INTx True 29
type UINT240 = INTx False 30;type INT240 = INTx True 30
type UINT248 = INTx False 31;type INT248 = INTx True 31
type UINT256 = INTx False 32;type INT256 = INTx True 32

-- | ABI bytes reference type.
newtype BYTES = BYTES ByteString

-- ABI value types

-- | ABI value type class.
class ABIValue a where
  from_svalue :: SVALUE -> a
  to_svalue :: a -> SVALUE

instance ABIValue ADDR where
  from_svalue (SVALUE a) = to_addr' a
  to_svalue (ADDR a) = SVALUE a

instance ABIValue BOOL where
  from_svalue (SVALUE 0) = false
  from_svalue (SVALUE _) = true

  to_svalue (BOOL False) = SVALUE 0
  to_svalue (BOOL True)  = SVALUE 1

instance (Typeable s, KnownNat n) => ABIValue (INTx s n) where
  from_svalue (SVALUE a) = fromIntegral a

  to_svalue (INT (Just a)) = SVALUE (naturalFromInteger a) -- assert (a <= max_sval)
  to_svalue (INT Nothing)  = def_sval

-- Num instance for INTx types.

instance Enum (INTx s n) where
  fromEnum (INT (Just a)) = fromEnum a
  fromEnum (INT Nothing)  =0
  toEnum = INT . Just . toEnum

-- | Num instance for the INTx types.
--
--
-- == Range check examples
-- >>> (to_intx 3 :: UINT8) + (to_intx 5 :: UINT8)
-- >>> (to_intx 255 :: UINT8) + (to_intx 1 :: UINT8)
-- >>> (to_intx 32 :: UINT8) * (to_intx 2 :: UINT8)
-- >>> (to_intx 32 :: UINT8) * (to_intx 8 :: UINT8)
-- INT (Just 8)
-- INT Nothing
-- INT (Just 64)
-- INT Nothing
--
-- >>> negate (to_intx 32 :: UINT8)
-- >>> negate (to_intx 32 :: INT8)
-- >>> to_intx (-128) :: INT8
-- >>> negate (to_intx (-128) :: INT8)
-- >>> abs (to_intx (-128) :: INT8)
-- INT Nothing
-- INT (Just (-32))
-- INT (Just (-128))
-- INT Nothing
-- INT Nothing
instance (Typeable s, KnownNat n) => Num (INTx s n) where
  (INT (Just a)) + (INT (Just b)) = fromInteger (a + b)
  _ + _                           = INT Nothing

  (INT (Just a)) * (INT (Just b)) = fromInteger (a * b)
  _ * _                           = INT Nothing

  negate (INT (Just a)) = fromInteger (negate a)
  negate _              = INT Nothing

  abs (INT (Just a)) = fromInteger (abs a)
  abs _              = INT Nothing

  signum (INT (Just a)) = INT (Just (signum a))
  signum _              = INT Nothing

  fromInteger a = let a' = INT (Just (fromInteger a))
                  in if a' >= min_intx @(INTx s n) && a' <= max_intx @(INTx s n)
                     then a' else INT Nothing

instance (Typeable s, KnownNat n) => Real (INTx s n) where
  toRational (INT (Just a)) = toRational a
  toRational (INT Nothing)  = toRational (0 :: Integer)

instance (Typeable s, KnownNat n) => Integral (INTx s n) where
  toInteger (INT (Just a)) = toInteger a
  toInteger (INT Nothing)  = toInteger (0 :: Integer)

  quotRem _              (INT (Just 0)) = (INT Nothing, INT Nothing)
  quotRem (INT (Just a)) (INT (Just b)) = let (c, d) = quotRem a b in (INT (Just c), INT (Just d))
  quotRem _              _              = (INT Nothing, INT Nothing)

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
