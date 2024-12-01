{-# OPTIONS_GHC -Wno-orphans #-}
module Data.MPOrd.YulDSL where

-- linear-base
import           Prelude.Linear (Bool (False, True), Consumable, lseq)
-- yul-dsl
import           YulDSL.Core
--
import           Data.MPOrd

-- | 'MPEq' instance for yul category morphisms.
instance (YulObj r, YulNum a) => MPEq (YulCat eff r a) (YulCat eff r BOOL) where
  (==) = (==?)
  (/=) = (/=?)

-- | 'MPOrd' instance for yul category morphisms.
instance (YulObj r, YulNum a) => MPOrd (YulCat eff r a) (YulCat eff r BOOL) where
  ( <) = ( <?)
  (<=) = (<=?)
  ( >) = ( >?)
  (>=) = (>=?)

-- | Default if-then-else instance for Haskell Bool.
instance Consumable a => IfThenElse Bool a where
  ifThenElse True  a b = lseq b a
  ifThenElse False a b = lseq a b
