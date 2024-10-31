{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|

Module      : Ethereum.ContractABI.CoreType.NP
Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : GHC2024

-}

module Ethereum.ContractABI.CoreType.NP
  ( NP (..)
  , NPHead, NPTail
  , showNP
  ) where

-- base
import           Data.Kind                        (Type)
import           Data.List                        (intercalate)
import           GHC.TypeLits                     (ErrorMessage (Text), TypeError)
-- constraints
import           Data.Constraint                  (Dict (Dict))
--
import           Ethereum.ContractABI.ABITypeable (ABITypeable (..))


data NP :: [Type] -> Type where
  Nil  :: NP '[]
  (:*) :: ABITypeable x => x -> NP xs -> NP (x : xs)

infixr 5 :*

data AnyNP = forall as. MkAnyNP (NP as)

type family NPHead (np :: [Type]) :: [Type] where
  NPHead (a:_) = '[a]
  NPHead '[] = TypeError (Text "NPHead '[] = _|_")

type family NPTail (np :: [Type]) :: [Type] where
  NPTail (_:as) = as
  NPTail '[] = '[]

instance ABITypeable (NP '[]) where
  type instance ABITypeDerivedOf (NP '[]) = (NP '[])
  abiTypeInfo = []

instance (ABITypeable x, ABITypeable (NP xs)) => ABITypeable (NP (x : xs)) where
  type instance ABITypeDerivedOf (NP (x : xs)) = (NP (x : xs))
  abiTypeInfo = abiTypeInfo @x <> abiTypeInfo @(NP xs)

-- | ABI typeable unit.
instance ABITypeable () where
  type ABITypeDerivedOf () = NP '[]
  abiToCoreType () = Nil
  abiFromCoreType Nil = ()

-- | ABI typeable tuple.
instance (ABITypeable a1, ABITypeable a2) => ABITypeable (a1, a2) where
  type ABITypeDerivedOf (a1, a2) = NP '[a1, a2]
  abiProdObjs = Dict
  abiToCoreType (a1, a2) = a1 :* a2 :* Nil
  abiFromCoreType (a1 :* a2 :* Nil) = (a1, a2)

instance Show (NP '[]) where
  show _ = "()"

instance Show x => Show (NP (x : xs)) where
  show as = "(" ++ intercalate "," (showNP (MkAnyNP as)) ++ ")"

-- | Show a NP as a list of strings.
showNP :: AnyNP -> [String]
showNP (MkAnyNP (Nil))     = []
showNP (MkAnyNP (a :* as)) = [show a] <> showNP (MkAnyNP as)
