{-# LANGUAGE TypeFamilies #-}

{-|

Module      : Ethereum.ContractABI.Type.NP
Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : GHC2024

-}

module Ethereum.ContractABI.CoreType.NP where

-- base
import           Data.Kind
--
import           Ethereum.ContractABI.ABITypeable (ABITypeable (ABITypeDerivedOf, abiTypeInfo))


data NP :: [Type] -> Type where
  Nil :: NP '[]
  (:*) :: x -> NP xs -> NP (x : xs)

infixr 5 :*

instance ABITypeable (NP '[]) where
  type instance ABITypeDerivedOf (NP '[]) = (NP '[])
  abiTypeInfo = []

instance (ABITypeable x, ABITypeable (NP xs)) => ABITypeable (NP (x : xs)) where
  type instance ABITypeDerivedOf (NP (x : xs)) = (NP (x : xs))

  abiTypeInfo = abiTypeInfo @x <> abiTypeInfo @(NP xs)
