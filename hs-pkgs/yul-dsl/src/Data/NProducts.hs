{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

Type-level programming for n-ary products and n-tuple.

Notes:

* The unsaturated type families support is missing currently in GHC, which results some of the acrobatics you will see
  below. See GHC proposal: https://github.com/ghc-proposals/ghc-proposals/pull/242.

* This may be better implemented using https://hackage.haskell.org/package/first-class-families.

-}

module Data.NProducts
  ( (:*) (..)
  , ScanNP, AtomizeNP, CountAtomsNP, HeadANP, TailANP
  , UnM
  ) where

import           Data.Kind    (Type)
import           GHC.TypeNats (Nat, type (+))

-- | Type constructor ':*' and its data constructor pun for creating currying n-ary products.
data a :* b = a :* b

-- | Operator (:*) being right associative allows bracket-free syntax.
infixr 5 :*

instance (Show a, Show b) => Show (a :* b) where
  show (a :* b) = "(" <> show a <> "," <> show b <> ")"

------------------------------------------------------------------------------------------------------------------------
-- Type-level Functions for the N-Ary Products (NP)
------------------------------------------------------------------------------------------------------------------------

-- | Scan through the n-ary products using the scanner @f@.
type family ScanNP (f :: k -> k -> k) (a :: k) :: k where
  ScanNP f (m (a1 :* a2)) = ScanNP f (m a1) `f` ScanNP f (m a2)
  ScanNP f (m a)          = m a

-- | "Atomize" the n-ary products.
type AtomizeNP ma = ScanNP (:*) ma

-- | Count number of "atoms" in the n-ary products.
type CountAtomsNP a = NatBuild (ScanNP NatPlus (NatConst (NatVal 1) a))

type family HeadANP (a :: Type) :: Type where
  HeadANP () = ()
  HeadANP (a1 :* a2) = a1
  HeadANP a = a

type family TailANP (a :: Type) :: Type where
  TailANP (a1 :* a2) = a2
  TailANP a = ()

-- Type level function utlities
--

type family UnM (ma :: Type) :: Type where UnM (m a) = a

data NatBuilder where
  NatPlus  :: NatBuilder -> NatBuilder -> NatBuilder
  NatConst :: forall a. NatBuilder -> a -> NatBuilder
  NatVal   :: Nat -> NatBuilder

type family NatBuild (n :: NatBuilder) :: Nat where
  NatBuild (NatPlus  a b) = NatBuild a + NatBuild b
  NatBuild (NatConst a _) = NatBuild a
  NatBuild (NatVal   a  ) = a
