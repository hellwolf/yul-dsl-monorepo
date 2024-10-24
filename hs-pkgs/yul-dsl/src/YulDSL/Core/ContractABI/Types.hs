{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies  #-}
{-|

Copyright   : (c) 2023-2004 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

This module includes the necessary primitive and composite types for any contract ABI types. Additionally, utility
functions and common type class instances (such as Num) are provided for them, so that interacting with them outside of
the EVM is possible.

One design principle of the library is to avoid any partial functions at all cost: it is reflected in the design of the
types and functions here.

-}

module YulDSL.Core.ContractABI.Types
  ( -- * ABI Type Core
    ABITypeInfo (..)
  , abi_type_uniq_name, abi_type_uniq_name'
  , abi_type_canon_name, abi_type_canon_name'
  , abi_type_count_vars, abi_type_eq

    -- ** ABI Serialization
  , ABISerialize, abi_encode, abi_decode

    -- ** ABI Types
  , ABIType (..)

    -- ** ABI Static Value Types
  , SVALUE, def_sval, max_sval
  , ABIValue(..)

    -- * Primitive Types

    -- ** BOOL
  , module Data.TypeBools
  , BOOL (..), true, false, if'

    -- ** ADDR
  , ADDR, zero_address, max_addr, to_addr, to_addr', addr_to_integer

    -- ** INTx
  , KnownNat
  , INTx, intx_sign, intx_nbits, min_intx, max_intx, to_intx

    -- *** Assorted INTx Types
  , UINT8,UINT16,UINT24,UINT32,UINT40,UINT48,UINT56,UINT64
  , UINT72,UINT80,UINT88,UINT96,UINT104,UINT112,UINT120,UINT128
  , UINT136,UINT144,UINT152,UINT160,UINT168,UINT176,UINT184,UINT192
  , UINT200,UINT208,UINT216,UINT224,UINT232,UINT240,UINT248,UINT256
  , INT8,INT16,INT24,INT32,INT40,INT48,INT56,INT64
  , INT72,INT80,INT88,INT96,INT104,INT112,INT120,INT128
  , INT136,INT144,INT152,INT160,INT168,INT176,INT184,INT192
  , INT200,INT208,INT216,INT224,INT232,INT240,INT248,INT256
    -- *** Range Check Examples
    -- $range_check_examples

    -- ** BYTES
  , BYTES(..)

    -- * Composite Types

  , module Data.NProducts
  , module Data.NTuple

  , FuncSig, Sel4Bytes, SEL (..), mkTypedSelector, mkRawSelector
  , FuncStorage (..), FuncEffect (..), FUNC (..)

    -- * Show Instance Examples
    -- $show_instance_examples
  ) where

-- base
import           Data.Bits             (shift, (.|.))
import           Data.List             (intercalate)
import           Data.Maybe            (fromJust)
import           Data.Proxy            (Proxy (..))
import           Data.Word             (Word32)
import           GHC.TypeNats          (KnownNat, Nat, SNat, natSing, natVal)
import           Numeric               (showHex)
-- bytestring
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
-- constraints
import           Data.Constraint       (Dict (..))
-- cereal
import qualified Data.Serialize        as S
-- crypton, memory
import           Crypto.Hash           (Digest, Keccak_256, hash)
import qualified Data.ByteArray        as BA
-- (this)
import           Data.NProducts
import           Data.NTuple
import           Data.TypeBools

------------------------------------------------------------------------------------------------------------------------
-- ABI Type Core
------------------------------------------------------------------------------------------------------------------------

-- | Contract ABI type information.
data ABITypeInfo where
  ADDR'   :: ABITypeInfo
  BOOL'   :: ABITypeInfo
  INTx'   :: forall s n. (KnownBool s, KnownNat n) => SBool s -> SNat n -> ABITypeInfo
  BYTESn' :: forall n. (KnownNat n) => SNat n -> ABITypeInfo
  BYTES'  :: ABITypeInfo
  -- ARRAY'  :: ABIType a => Proxy a -> ABITypeInfo

-- | Unambiguous succinct and unique name for the 'ABITypeInfo'.
abi_type_uniq_name':: ABITypeInfo -> String
abi_type_uniq_name' ADDR'       = "a"
abi_type_uniq_name' BOOL'       = "b"
abi_type_uniq_name' (INTx' s n) = if toBool s then "i" else "u" <> show (natVal n)
abi_type_uniq_name' (BYTESn' n) = "B" ++ show (natVal n)
abi_type_uniq_name' BYTES'      = "Bs"

-- | Unambiguous succinct and unique name for the 'ABIType'.
abi_type_uniq_name :: forall a. ABIType a => String
abi_type_uniq_name = intercalate "" $ fmap abi_type_uniq_name' (abi_type_info @a)

-- | Canonical name used as argument of the selector for the 'ABITypeInfo'.
abi_type_canon_name' :: ABITypeInfo -> String
abi_type_canon_name' ADDR'       = "address"
abi_type_canon_name' BOOL'       = "bool"
abi_type_canon_name' (INTx' s n) = if toBool s then "int" else "uint" <> show (natVal n * 8)
abi_type_canon_name' (BYTESn' n) = "bytes" <> show (natVal n)
abi_type_canon_name' BYTES'      = "bytes"

-- | Canonical name used as argument of the selector for the 'ABIType'.
abi_type_canon_name :: forall a. ABIType a =>  String
abi_type_canon_name = intercalate "," $ fmap abi_type_canon_name' (abi_type_info @a)

-- | Number of yul primitive variables required to capture the value of this type.
abi_type_count_vars :: forall a. ABIType a => Int
abi_type_count_vars = length (abi_type_info @a)

-- | Check type equality of ABIType @a@ and ABIType @b@.
abi_type_eq :: forall a b. (ABIType a, ABIType b) => Bool
abi_type_eq = abi_type_uniq_name @a == abi_type_uniq_name @b

-- | Contract ABI type class for all primitive and composite ABI types.
class (Show a) => ABIType a where
  -- | Possible breakdown of the product object type.
  abi_prod_objs :: forall b c. a ~ (b, c) => Dict (ABIType b, ABIType c)
  -- ^ Default implementation returns (a, ())
  abi_prod_objs = error "abi_prod_objs should only be implemented by the product of ABIType"

  -- | Returns a list of ABI type information represented by this ABI type.
  abi_type_info :: [ABITypeInfo]

  -- | List of yul expressions required to capture the value of this type.
  abi_type_list_vars :: a -> [String]

-- | Raw storage value for ABI value types.
newtype SVALUE = SVALUE Integer deriving newtype (Eq, Show)

-- | Default storage value.
def_sval :: SVALUE
def_sval = SVALUE 0

-- | Maximum storage value.
max_sval :: SVALUE
max_sval = SVALUE (2 ^ (256 :: Int) - 1)

-- | ABI (static) value types.
class ABIType a => ABIValue a where
  -- | Convert from a storage value to an ABI typed value.
  from_svalue :: SVALUE -> a
  -- | Convert from a ABI typed value to a storage value.
  to_svalue :: a -> SVALUE

-- | Unit type of ABI Types
instance ABIType () where
  abi_type_info = []
  abi_type_list_vars _ = []

------------------------------------------------------------------------------------------------------------------------
-- * Primitive Types
------------------------------------------------------------------------------------------------------------------------

-- ** ADDR
--

-- | ABI address value type.
newtype ADDR = ADDR Integer deriving newtype (Ord, Eq)

instance ABIType ADDR where
  abi_type_info = [ADDR']
  abi_type_list_vars a = [show a]

instance ABIValue ADDR where
  from_svalue (SVALUE a) = to_addr' a
  to_svalue (ADDR a) = SVALUE a

instance Show ADDR where
  show (ADDR a) = "0x" ++ _lpad0 40 (showHex a) ""

-- | The proverbial zero address.
zero_address :: ADDR
zero_address = ADDR 0

_max_addr_nat :: Integer
_max_addr_nat = (2 ^ (256 :: Int)) - 1

-- | Maximum value of address.
max_addr :: ADDR
max_addr = ADDR _max_addr_nat

-- | From integer to ADDR.
to_addr :: Integer -> Maybe ADDR
to_addr a = if a >= 0 && a <= _max_addr_nat then Just (ADDR a) else Nothing

to_addr' :: Integer -> ADDR
to_addr' = fromJust . to_addr

-- | Convert address to integer.
addr_to_integer :: ADDR -> Integer
addr_to_integer (ADDR a) = a

-- * BOOL
--

-- | ABI boolean value type.
newtype BOOL = BOOL Bool deriving newtype (Eq)

instance ABIType BOOL where
  abi_type_info = [BOOL']
  abi_type_list_vars a = [show a]

instance ABIValue BOOL where
  from_svalue (SVALUE 0) = false
  from_svalue (SVALUE _) = true

  to_svalue (BOOL False) = SVALUE 0
  to_svalue (BOOL True)  = SVALUE 1

instance Show BOOL where
  show (BOOL True)  = "true"
  show (BOOL False) = "false"

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

-- * INTx
--

-- | ABI integer value types, where @s@ is for signess and @n@ is the multiple of 8 bits
newtype INTx (s :: Bool) (n :: Nat) = INT (Maybe Integer)

instance Eq (INTx s n) where
  INT (Just a) == INT (Just b) = a == b
  INT Nothing == INT Nothing   = True
  INT Nothing == INT (Just _)  = False
  INT (Just _) == INT Nothing  = False

instance Ord (INTx s n) where
  INT (Just a) <= INT (Just b) = a <= b
  INT Nothing <= INT Nothing   = True
  INT Nothing <= INT (Just _)  = False
  INT (Just _) <= INT Nothing  = False

instance forall s n. (KnownBool s, KnownNat n) => ABIType (INTx s n) where
  abi_type_info = [INTx' (SBool @s) (natSing @n)]
  abi_type_list_vars a = [show a]

instance (KnownBool s, KnownNat n) => ABIValue (INTx s n) where
  from_svalue (SVALUE a) = fromIntegral a -- FIXME, support signed numbers

  to_svalue (INT (Just a)) = SVALUE a -- FIXME, support signed numbers
  to_svalue (INT Nothing)  = def_sval

instance (KnownBool s, KnownNat n) => Show (INTx s n) where
  show (INT (Just a)) = show a
  show (INT Nothing)  = "NaN"

instance Enum (INTx s n) where
  fromEnum (INT (Just a)) = fromEnum a
  fromEnum (INT Nothing)  = 0
  toEnum = INT . Just . toEnum

instance (KnownBool s, KnownNat n) => Real (INTx s n) where
  toRational (INT (Just a)) = toRational a
  toRational (INT Nothing)  = toRational (0 :: Integer)

instance (KnownBool s, KnownNat n) => Integral (INTx s n) where
  toInteger (INT (Just a)) = toInteger a
  toInteger (INT Nothing)  = toInteger (0 :: Integer)

  quotRem _              (INT (Just 0)) = (INT Nothing, INT Nothing)
  quotRem (INT (Just a)) (INT (Just b)) = let (c, d) = quotRem a b in (INT (Just c), INT (Just d))
  quotRem _              _              = (INT Nothing, INT Nothing)

instance (KnownBool s, KnownNat n) => Num (INTx s n) where
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

instance (KnownBool s, KnownNat n) => Bounded (INTx s n) where
  minBound = min_intx
  maxBound = max_intx

-- | Minimum value of the INTx type. Use type application on @a@.
min_intx :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, KnownBool s, KnownNat n) => a
min_intx = INT . Just $
  if intx_sign @(INTx s n) then negate (1 `shift` (nbits - 1)) else 0
  where nbits = intx_nbits @(INTx s n)

-- | Maximum value of the INTx type. Use type application on @a@.
max_intx :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, KnownBool s, KnownNat n) => a
max_intx = INT . Just $
  if intx_sign @(INTx s n) then (1 `shift` (nbits - 1)) - 1 else (1 `shift` nbits) - 1
  where nbits = intx_nbits @(INTx s n)

-- | Sign of the INTx type. Use type application on @a@.
intx_sign :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, KnownBool s, KnownNat n) => Bool
intx_sign = toBool (SBool @s)

-- | Number of bits for the INTx type. Use type application on @a@.
intx_nbits :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, KnownNat n) => Int
intx_nbits = fromEnum (8 * natVal (Proxy @n))

-- | Show the canonical type name for the INTX type. Use type application on @a@.
-- intx_typename :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, KnownBool s, KnownNat n) => String
-- intx_typename = (if intx_sign @a then "" else "U") ++ "INT" ++ show (intx_nbits @a)

-- | Convert integer to the INTx type.
to_intx :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, KnownBool s, KnownNat n) => Integer -> a
to_intx = fromIntegral

-- Assorted integer types:
--
--   (Note: Code generation command)
--   sh$ for i in `seq 1 32`;do echo "type UINT$((i*8)) = INTx False $i;type INT$((i*8)) = INTx True $i";done

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

-- * BYTES
-- TODO

-- | ABI bytes reference type.
newtype BYTES = BYTES B.ByteString deriving newtype (Eq)

instance ABIType BYTES where
  abi_type_info = [BYTES']
  abi_type_list_vars a = [show a]

instance Show BYTES where
  show (BYTES a) = "0x" ++ (foldr (_lpad0 2 . showHex) "" . B.unpack) a

------------------------------------------------------------------------------------------------------------------------
-- External Function Types
------------------------------------------------------------------------------------------------------------------------

-- * SEL
--

-- | External function signature. This optional information does not have run-time representation.
type FuncSig = Maybe (String {- function name -}, String {- argument types -})

-- | Selector value type.
type Sel4Bytes = INTx False 4

-- | Selector value type with the optional function signature tagged.
newtype SEL = SEL (Sel4Bytes, FuncSig)

instance ABIType SEL where
  abi_type_info = [BYTESn' (natSing @4)]
  abi_type_list_vars (SEL (_, b))  = [show b]
--  abi_type_show_vars (SEL (Just (sig, args), _)) = [sig]

instance Show SEL where
  show (SEL (sig, Just (fname, argTypes))) = "0x" ++ showHex sig " /* " ++  fname ++ "(" ++ argTypes ++ ") */"
  show (SEL (sig, Nothing))                = "0x" ++ showHex sig ""

-- Generate ABI Contract compatible signature string.
make_sig :: forall a. ABIType a => String -> String
make_sig fname = fname <> "(" <> abi_type_canon_name @a <> ")"

-- | Create a solidity-compatible selector based on types.
--
mkTypedSelector :: forall a. ABIType a => String -> SEL
mkTypedSelector fname = SEL (fromIntegral sel4bytes, Just (fname, abi_type_canon_name @a))
  where
    sig = make_sig @a fname
    bs4bytes = B.unpack(B.take 4(BA.convert(hash(BC.pack sig) :: Digest Keccak_256) :: B.ByteString))
    sel4bytes = (fromIntegral (bs4bytes!!3) :: Word32)
      .|. shift (fromIntegral (bs4bytes!!2) :: Word32) 8
      .|. shift (fromIntegral (bs4bytes!!1) :: Word32) 16
      .|. shift (fromIntegral (bs4bytes!!0) :: Word32) 24

mkRawSelector :: Sel4Bytes -> SEL
mkRawSelector sig = SEL (sig, Nothing)

-- | Create a solidity-compatible selector based on the plain signature.
-- sigToSelector :: String -> SEL
-- mkSelector s = SEL (Just s, 0) -- FIXME Signature parsing

-- * FUNC

-- | Storage location for the external function call.
data FuncStorage = FuncExternal | FuncDelegated

-- | Effect type for the external function call.
data FuncEffect = FuncTx | FuncStatic

-- | External function specification.
newtype FUNC a b = FUNC (FuncStorage, FuncEffect, SEL, ADDR)

instance forall a b. (ABIType a, ABIType b) => ABIType (FUNC a b) where
  abi_type_info = [BYTESn' (natSing @24)]
  abi_type_list_vars a = [show a]

instance Show (FUNC a b) where
  show _              = error "TODO Show FUNC"

------------------------------------------------------------------------------------------------------------------------
-- Tuple Types (Product, N-ary Products & N-Tuples)
------------------------------------------------------------------------------------------------------------------------

instance forall a b. (ABIType a, ABIType b) => ABIType (a, b) where
  abi_prod_objs = Dict
  abi_type_info = abi_type_info @a <> abi_type_info @b
  abi_type_list_vars (a, b) = abi_type_list_vars a <> abi_type_list_vars b

-- | ABIType (a :* b) is equivalent to ABIType (a, b).
instance forall a b. (ABIType a, ABIType b) => ABIType (a :* b) where
  abi_type_info = abi_type_info @(a, b)
  abi_type_list_vars (a :* b) = abi_type_list_vars (a, b)

{- INTERNAL FUNCTIONS -}

_lpad0 :: Int -> ShowS -> ShowS
_lpad0 n s = showString (reverse (take n (reverse (s "") ++ repeat '0')))

{- $range_check_examples

__INTx Types__

>>> (min_intx @INT32, max_intx @INT32)
>>> (min_intx @UINT32, max_intx @UINT32)
>>> (min_intx @INT96, max_intx @INT96)
>>> to_intx (-128) :: INT8
>>> to_intx 128 :: INT8
(-2147483648,2147483647)
(0,4294967295)
(-39614081257132168796771975168,39614081257132168796771975167)
-128
NaN

__Num Operators__

>>> (to_intx 3 :: UINT8) + (to_intx 5 :: UINT8)
>>> (to_intx 255 :: UINT8) + (to_intx 1 :: UINT8)
>>> (to_intx 32 :: UINT8) * (to_intx 2 :: UINT8)
>>> (to_intx 32 :: UINT8) * (to_intx 8 :: UINT8)
8
NaN
64
NaN

>>> negate (to_intx 32 :: UINT8)
>>> negate (to_intx 32 :: INT8)
>>> negate (to_intx (-128) :: INT8)
>>> abs (to_intx (-128) :: INT8)
NaN
-32
NaN
NaN

>>> show (SEL (Nothing, 69))
>>> show (SEL (Just ("foo", ""), 0)) -- TODO 4bytes needs to be generated
Couldn't match type: Maybe a0_a7sS4[tau:1]
               with: INTx 'False 4
Expected: Sel4Bytes
  Actual: Maybe a0_a7sS4[tau:1]
In the expression: Nothing
In the first argument of `SEL', namely `(Nothing, 69)'
In the first argument of `show', namely `(SEL (Nothing, 69))'
-}

{- $show_instance_examples

>>> show true
>>> show (ADDR 0x42)
>>> show (to_intx 255 :: UINT8)
>>> show (to_intx (-8) :: INT96)
>>> show (to_intx 256 :: UINT8)
>>> show (BYTES (BC.pack "hello, world"))
>>> show (BYTES (B.pack [4, 2]))
"true"
"0x0000000000000000000000000000000000000042"
"255"
"-8"
"NaN"
"0x68656c6c6f2c20776f726c64"
"0x0402"
-}


------------------------------------------------------------------------------------------------------------------------
-- Serialization
--
-- TODO implement Contract ABI compatible serialization.
------------------------------------------------------------------------------------------------------------------------

-- | ABI serialization class. FIXME: currently an alias to Serialize from cereal library.
type ABISerialize = S.Serialize

-- | ABI encoder.
abi_encode :: ABISerialize a => a -> B.ByteString
abi_encode = S.encode

-- | ABI decoder.
abi_decode :: ABISerialize a => B.ByteString -> Maybe a
abi_decode a = case S.decode a of
                 Right b -> Just b
                 Left _  -> Nothing
