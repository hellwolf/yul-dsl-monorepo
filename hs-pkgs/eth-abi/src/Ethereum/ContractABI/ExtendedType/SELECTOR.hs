{-# LANGUAGE AllowAmbiguousTypes #-}
module Ethereum.ContractABI.ExtendedType.SELECTOR
 ( FuncSig (FuncSig)
 , SELECTOR (SELECTOR)
 , makeFuncSig
 , mkTypedSelector
 , mkRawSelector
 ) where

-- base
import Data.List                            (intercalate)
-- eth-abi
import Ethereum.ContractABI.ABICoreType
import Ethereum.ContractABI.ABITypeable
import Ethereum.ContractABI.CoreType.BYTESn


-- | External function signature. This optional information does not have run-time representation.
newtype FuncSig = FuncSig (String {- function name -}, [String] {- argument types -})

instance Show FuncSig where
  show (FuncSig (fname, args)) = fname ++ "(" ++ intercalate "," args ++ ")"

-- | Selector value type with the optional function signature tagged.
newtype SELECTOR = SELECTOR (B4, Maybe FuncSig)

-- | Generate ABI Contract compatible signature string.
makeFuncSig :: forall a. ABITypeable a => String -> FuncSig
makeFuncSig fname = FuncSig (fname, fmap abiCoreTypeCanonName (abiTypeInfo @a))

-- | Create a selector from a function name @fname@ and its input type signature @a@.
mkTypedSelector :: forall a. ABITypeable a => String -> SELECTOR
mkTypedSelector fname = SELECTOR (bytesnFromWord8s bs4bytes, Just (makeFuncSig @a fname))
  where
    sig = show (makeFuncSig @a fname)
    bs4bytes = take 4 (unBYTESn (stringKeccak256 sig))

-- | Create a selector from a raw 'U32' value.
mkRawSelector :: B4 -> SELECTOR
mkRawSelector sig = SELECTOR (sig, Nothing)

instance Show SELECTOR where
  show (SELECTOR (sig, Just (FuncSig (fname, args)))) = show sig ++
    " /* " ++ fname ++ "(" ++ intercalate ","args ++ ") */"
  show (SELECTOR (sig, Nothing)) = show sig
