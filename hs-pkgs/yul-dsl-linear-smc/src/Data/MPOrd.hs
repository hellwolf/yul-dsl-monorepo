{-# LANGUAGE FunctionalDependencies #-}
module Data.MPOrd where

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
