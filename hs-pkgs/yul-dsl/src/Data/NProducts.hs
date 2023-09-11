{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

-}

module Data.NProducts
  ( (:*) (..)
  , ScanNP
  , AtomizeNP, CountAtomsNP
  ) where

import           GHC.TypeNats (Nat, type (+))

-- | Type constructor ':*' and its data constructor pun for creating currying n-ary products.
data a :* b = a :* b

-- | Operator (:*) being right associative allows bracket-free syntax.
infixr :*

instance (Show a, Show b) => Show (a :* b) where
  show (a :* b) = "(" <> show a <> "," <> show b <> ")"

------------------------------------------------------------------------------------------------------------------------
-- Type-level Functions for the N-Ary Products
--
-- Notes:
--
-- * The unsaturated type families support is missing currently in GHC, which results some of the acrobatics you will
--   see below. See GHC proposal: https://github.com/ghc-proposals/ghc-proposals/pull/242.
--
-- * This may be better implemented using https://hackage.haskell.org/package/first-class-patterns.
------------------------------------------------------------------------------------------------------------------------

-- | Scan through the n-ary products using the scanner @c@.
type family ScanNP (c :: k -> k -> k) (a :: k) :: k where
  ScanNP c (m (a1 :* a2)) = ScanNP c (m a1) `c` ScanNP c (m a2)
  ScanNP c (m a)          = m a

-- | "Atomize" the n-ary products.
type AtomizeNP ma = ScanNP (:*) ma

-- | Count number of "atoms" in the n-ary products.
type CountAtomsNP a = NatBuild (ScanNP NatPlus (NatConst (NatVal 1) a))

data NatBuilder where
  NatPlus  :: NatBuilder -> NatBuilder -> NatBuilder
  NatConst :: forall a. NatBuilder -> a -> NatBuilder
  NatVal   :: Nat -> NatBuilder

type family NatBuild (n :: NatBuilder) :: Nat where
  NatBuild (NatPlus  a b) = NatBuild a + NatBuild b
  NatBuild (NatConst a _) = NatBuild a
  NatBuild (NatVal   a  ) = a
