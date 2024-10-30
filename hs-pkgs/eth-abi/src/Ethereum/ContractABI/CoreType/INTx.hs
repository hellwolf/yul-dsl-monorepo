{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

{-|

Module      : Ethereum.ContractABI.Type.INTx
Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : GHC2024

-}

module Ethereum.ContractABI.CoreType.INTx where

-- base
import           Data.Bits                        (shift)
import           Data.Coerce                      (coerce)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.TypeBools                   (KnownBool (..), SBool (SBool))
import           GHC.TypeNats
-- eth-abi
import           Ethereum.ContractABI.ABICoreType (ABICoreType (INTx'), ABIWordValue (..), word, wordVal)
import           Ethereum.ContractABI.ABITypeable (ABITypeable (..))


-- | ABI integer value types, where @s@ is for signess and @n@ is the multiple of 8 bits
newtype INTx (s :: Bool) (n :: Nat) = INT Integer deriving (Eq, Ord, Enum, Show)

intxVal :: INTx s n -> Integer
intxVal (INT x) = x

-- | Sign of the INTx type. Use type application on @a@.
intxSign :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, KnownBool s, KnownNat n) => Bool
intxSign = toBool (SBool @s)

-- | Number of bits for the INTx type. Use type application on @a@.
intxNBits :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, KnownNat n) => Int
intxNBits = fromEnum (8 * natVal (Proxy @n))

instance forall s n. (KnownBool s, KnownNat n) => ABITypeable (INTx s n) where
  type instance ABITypeDerivedOf (INTx s n) = INTx s n

  abiTypeInfo = [INTx' (SBool @s) (natSing @n)]

{-  Num hierarchy classes -}

instance (KnownBool s, KnownNat n) => Bounded (INTx s n) where
  minBound = INT $
    if intxSign @(INTx s n) then negate (1 `shift` (nbits - 1)) else 0
    where nbits = intxNBits @(INTx s n)
  maxBound = INT $
    if intxSign @(INTx s n) then (1 `shift` (nbits - 1)) - 1 else (1 `shift` nbits) - 1
    where nbits = intxNBits @(INTx s n)

instance (KnownBool s, KnownNat n) => Num (Maybe (INTx s n)) where
  (Just (INT a)) + (Just (INT b)) = fromInteger (a + b)
  _ + _                           = Nothing

  (Just (INT a)) * (Just (INT b)) = fromInteger (a * b)
  _ * _                           = Nothing

  negate (Just (INT a)) = fromInteger (negate a)
  negate _              = Nothing

  abs (Just (INT a)) = fromInteger (abs a)
  abs _              = Nothing

  signum (Just (INT a)) = Just (INT (signum a))
  signum _              = Nothing

  fromInteger a = let a' = INT (fromInteger a)
                  in if a' >= minBound @(INTx s n) && a' <= maxBound @(INTx s n)
                     then Just a' else Nothing

instance Enum (Maybe (INTx s n)) where
  fromEnum (Just (INT a)) = fromEnum a
  fromEnum Nothing        = error "INTx.fromEnum Nothing"
  toEnum = Just . INT . toEnum

instance (KnownBool s, KnownNat n) => Integral (Maybe (INTx s n)) where
  toInteger (Just (INT a)) = toInteger a
  toInteger Nothing        = error "INTx.fromInteger Nothing"

  quotRem _              (Just (INT 0)) = (Nothing, Nothing)
  quotRem (Just (INT a)) (Just (INT b)) = let (c, d) = quotRem a b in (Just (INT c), Just (INT d))
  quotRem _              _              = (Nothing, Nothing)

instance (KnownBool s, KnownNat n) => Real (Maybe (INTx s n)) where
  toRational (Just (INT a)) = toRational a
  toRational Nothing        = error "INTx.toRational Nothing"

instance (KnownBool s, KnownNat n) => ABIWordValue (INTx s n) where
  fromWord w = let maxVal = coerce (maxBound @(INTx s n))
                   -- min = coerce (minBound @(INTx s n))
                   a  = wordVal w
               in if intxSign @(INTx s n)
                  then if a <= maxVal
                       then Just (INT a)
                       else let a' = a - (1 `shift` intxNBits @(INTx s n))
                            in if a' < 0 then Just (INT a') else Nothing
                  else if a <= maxVal
                       then Just (INT a)
                       else Nothing

  toWord (INT a) = if intxSign @(INTx s n)
                   then if a >= 0
                        then word a
                        else word (a + 1 `shift` intxNBits @(INTx s n))
                   else word a

{- Assorted Fixed-Precision Integer Aliases -}

-- Note: Code generation command
-- sh$ for i in `seq 1 32`;do echo "type U$((i*8)) = INTx False $i;type I$((i*8)) = INTx True $i";done

type U8 = INTx False 1;type I8 = INTx True 1
type U16 = INTx False 2;type I16 = INTx True 2
type U24 = INTx False 3;type I24 = INTx True 3
type U32 = INTx False 4;type I32 = INTx True 4
type U40 = INTx False 5;type I40 = INTx True 5
type U48 = INTx False 6;type I48 = INTx True 6
type U56 = INTx False 7;type I56 = INTx True 7
type U64 = INTx False 8;type I64 = INTx True 8
type U72 = INTx False 9;type I72 = INTx True 9
type U80 = INTx False 10;type I80 = INTx True 10
type U88 = INTx False 11;type I88 = INTx True 11
type U96 = INTx False 12;type I96 = INTx True 12
type U104 = INTx False 13;type I104 = INTx True 13
type U112 = INTx False 14;type I112 = INTx True 14
type U120 = INTx False 15;type I120 = INTx True 15
type U128 = INTx False 16;type I128 = INTx True 16
type U136 = INTx False 17;type I136 = INTx True 17
type U144 = INTx False 18;type I144 = INTx True 18
type U152 = INTx False 19;type I152 = INTx True 19
type U160 = INTx False 20;type I160 = INTx True 20
type U168 = INTx False 21;type I168 = INTx True 21
type U176 = INTx False 22;type I176 = INTx True 22
type U184 = INTx False 23;type I184 = INTx True 23
type U192 = INTx False 24;type I192 = INTx True 24
type U200 = INTx False 25;type I200 = INTx True 25
type U208 = INTx False 26;type I208 = INTx True 26
type U216 = INTx False 27;type I216 = INTx True 27
type U224 = INTx False 28;type I224 = INTx True 28
type U232 = INTx False 29;type I232 = INTx True 29
type U240 = INTx False 30;type I240 = INTx True 30
type U248 = INTx False 31;type I248 = INTx True 31
type U256 = INTx False 32;type I256 = INTx True 32
