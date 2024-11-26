{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.MPOrd.YulDSL.LinearSMC where

-- linear-base
import           Prelude.Linear (Consumable, lseq)
-- yul-dsl
import           YulDSL.Core


-- | Multi-parameter equality type class where boolean type is @b@
class MPEq a b | a -> b where
  (==) :: forall w. a %w -> a %w -> b
  (/=) :: forall w. a %w -> a %w -> b

-- | Multi-parameter ordering type class where boolean type is @b@
class MPEq a b => MPOrd a b | a -> b where
  ( <) :: forall w. a %w -> a %w -> b
  (<=) :: forall w. a %w -> a %w -> b
  ( >) :: forall w. a %w -> a %w -> b
  (>=) :: forall w. a %w -> a %w -> b

infixr 4 <, <=, >, >=, ==, /=

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
