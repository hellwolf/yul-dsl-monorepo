{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}
{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : hellwolf@yolc.dev
Stability   : experimental
Portability : GHC2024


= Description

Ethereum contract ABI assorted integer types.

-}

module Ethereum.ContractABI.CoreType.INTx
  ( INTx, ValidINTx, intxSign, intxNBits
    -- *** Assorted INTx Types
  , U8,U16,U24,U32,U40,U48,U56,U64
  , U72,U80,U88,U96,U104,U112,U120,U128
  , U136,U144,U152,U160,U168,U176,U184,U192
  , U200,U208,U216,U224,U232,U240,U248,U256
  , I8,I16,I24,I32,I40,I48,I56,I64
  , I72,I80,I88,I96,I104,I112,I120,I128
  , I136,I144,I152,I160,I168,I176,I184,I192
  , I200,I208,I216,I224,I232,I240,I248,I256
  ) where

-- base
import           Control.Monad                     (forM)
import           Data.Bits                         (shift)
import           Data.Coerce                       (coerce)
import           Data.Maybe                        (fromJust)
import           Data.Proxy                        (Proxy (Proxy))
-- template-haskell
import qualified Language.Haskell.TH               as TH
-- cereal
import qualified Data.Serialize                    as S
-- eth-abi
import           Ethereum.ContractABI.ABICoreType
import           Ethereum.ContractABI.ABITypeable
import           Ethereum.ContractABI.ABITypeCodec
import           Internal.Data.Type.Bool


-- | ABI integer value types, where @s@ is for signess and @n@ is the multiple of 8 bits
newtype INTx (s :: Bool) (n :: Nat) = INT Integer
  deriving newtype (Eq, Ord, Enum)

-- | A constraint alias for 'KnownBool' and 'ValidINTn'.
type ValidINTx s n = (KnownBool s, ValidINTn n)

-- | Sign of the INTx type. Use type application on @a@.
intxSign :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, ValidINTx s n) => Bool
intxSign = toBool (SBool @s)

-- | Number of bits for the INTx type. Use type application on @a@.
intxNBits :: forall a (s :: Bool) (n :: Nat). (a ~ INTx s n, ValidINTn n) => Int
intxNBits = fromEnum (8 * natVal (Proxy @n))

{- * Type class instances -}

instance forall s n. ValidINTx s n => ABITypeable (INTx s n) where
  type instance ABITypeDerivedOf (INTx s n) = INTx s n
  abiTypeInfo = [INTx' (SBool @s) (natSing @n)]

instance forall s n. ValidINTx s n => ABITypeCodec (INTx s n) where
  abiEncoder (INT x) = S.put x
  abiDecoder = fmap INT S.get

{- **  Num hierarchy classes for (Maybe INTx s n) -}

instance ValidINTx s n => Bounded (Maybe (INTx s n)) where
  minBound = Just . INT $
    if intxSign @(INTx s n) then negate (1 `shift` (nbits - 1)) else 0
    where nbits = intxNBits @(INTx s n)
  maxBound = Just . INT $
    if intxSign @(INTx s n) then (1 `shift` (nbits - 1)) - 1 else (1 `shift` nbits) - 1
    where nbits = intxNBits @(INTx s n)

instance ValidINTx s n => Num (Maybe (INTx s n)) where
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

instance ValidINTx s n => Real (Maybe (INTx s n)) where
  toRational (Just (INT a)) = toRational a
  toRational Nothing        = error "INTx.toRational Nothing"

instance Enum (Maybe (INTx s n)) where
  fromEnum (Just (INT a)) = fromEnum a
  fromEnum Nothing        = error "INTx.fromEnum Nothing"
  toEnum = Just . INT . toEnum

instance ValidINTx s n => Integral (Maybe (INTx s n)) where
  toInteger (Just (INT a)) = a
  toInteger Nothing        = error "INTx.fromInteger Nothing"

  quotRem _              (Just (INT 0)) = (Nothing, Nothing)
  quotRem (Just (INT a)) (Just (INT b)) = let (c, d) = quotRem a b in (Just (INT c), Just (INT d))
  quotRem _              _              = (Nothing, Nothing)

{-  ** Num hierarchy classes for (INTx s n) -}

instance ValidINTx s n => Bounded (INTx s n) where
  minBound = fromJust minBound
  maxBound = fromJust maxBound

instance ValidINTx s n => Num (INTx s n) where
  a + b = fromJust (Just a + Just b)
  a * b = fromJust (Just a * Just b)
  abs = fromJust . abs . Just
  signum = fromJust . signum . Just
  fromInteger = fromJust . fromInteger
  negate = fromJust . negate . Just

instance ValidINTx s n => Real (INTx s n) where
  toRational = toRational . Just

instance ValidINTx s n => Integral (INTx s n) where
  toInteger = toInteger . Just
  quotRem a b = let (a', b') = quotRem (Just a) (Just b) in (fromJust a', fromJust b')

{- ** ABIWordValue instances -}

instance ValidINTx s n => ABIWordValue (INTx s n) where
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

{- ** Show instances -}

instance ValidINTx s n => Show (INTx s n) where
  show (INT a) = show a

{- * Assorted Fixed-Precision Integer Aliases -}

-- Note: Code generation command
-- sh$ for i in `seq 1 32`;do echo "type U$((i*8)) = INTx False $i;type I$((i*8)) = INTx True $i";done

-- Alternatively: we can use top-level TH splice to generate these. But it confused LSP, for now.
--
forM [ (s, n) | s <- [True, False], n <- [1..32] ] $ \(s, n) -> do
  name <- TH.newName ((if s then "I" else "U") ++ show (n * 8))
  TH.tySynD name [] ((TH.conT ''INTx)
                      `TH.appT` (TH.promotedT (if s then 'True else 'False))
                      `TH.appT` (TH.litT (TH.numTyLit n)))
