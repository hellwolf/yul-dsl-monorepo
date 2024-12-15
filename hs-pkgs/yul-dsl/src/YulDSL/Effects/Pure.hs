{-# LANGUAGE AllowAmbiguousTypes #-}
{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : hellwolf@yolc.dev
Stability   : experimental

This module provides the operations for working with the 'Pure' kind of effect for the yul category morphisms.

-}
module YulDSL.Effects.Pure
  ( Pure (MkPure), PureFn, YulCat'P
  , uncurry'p, fn'p, fn
  , call'p
  ) where

-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.Fn
import           YulDSL.Core.YulCat
import           YulDSL.Core.YulCatObj


-- $yulCatVal
--
-- A yul categorical value of @r ↝ a@ is another way of saying all morphisms that leads to @a@ in the category of 'YulCat'.
--
-- One may also wrap it around an effect kind, e.g. @Pure (r ↝ a)@ means a pure yul categorical value of @r ↝ a@.
--
-- From category theory perspective, it is a hom-set @YulCat(-, a)@ that is contravariant of @a@.

-- | Data kinds for pure morphisms in the yul category.
data Pure = MkPure

-- | Pure effect is not non-pure. :)
type instance NonPureEffect MkPure = False

-- | Function without side effects, hence pure.
type PureFn = Fn MkPure

-- | Pure yul category morphisms.
type YulCat'P = YulCat MkPure

-- | Uncurry a currying function @f@ with pure yul categorical values of @NP xs ↝ x_n@ that returns a pure categorical
-- value @NP xs ↝ b@.
--
-- = Note: How to Read This Type Signature
-- When given:
--
--   * @NP xs = (x1, x2 ... xn)@
--   * @x1' = Pure (NP xs ↝ x1), x2' = Pure (NP xs ↝ x2), ... xn' = Pure (NP xs ↝ xn)@
--   * @f = λ x1' -> λ x2' -> ... λ xn' -> Pure (NP xs ↝ b)@
--
-- It returns: @Pure (NP xs ↝ b)@
--
-- = Note: How to use
-- @
--   -- Use type application to resolve the type @f@:
--   bar = fn'p "" $ uncurry'p @(U256 -> U256)
--     \a -> a + a
-- @
uncurry'p :: forall f xs b m.
             ( YulO2 (NP xs) b
             , UncurryNP'Fst f ~ xs
             , UncurryNP'Snd f ~ b
             , YulCat'P (NP xs) ~ m
             , UncurryingNP f xs b m m m m Many
             , LiftFunction b m m Many ~ m b
             )
          => LiftFunction f m m Many -- ^ uncurrying function type
          -> YulCat'P (NP xs) b      -- ^ result type, or its short form @m b@
uncurry'p f = uncurryingNP @f @xs @b @m @m @m @m f YulId

-- | Create a `PureFn` given a function id and a pure categorical value @NP xs ↝ b@
fn'p :: forall f xs b.
        ( YulO2 (NP xs) b
        , CurryNP (NP xs) b ~ f
        , UncurryNP'Fst f ~ xs
        , UncurryNP'Snd f ~ b
        )
     => String                     -- ^ the function id
     -> YulCat'P (NP xs) b         -- ^ the pure yul categorical value of @NP xs ↝ b@
     -> PureFn (CurryNP (NP xs) b) -- ^ a 'PureFn' of function type @f@
fn'p fid cat'l = MkFn (MkFnCat fid cat'l)

-- | An alias for creating 'PureFn' using 'uncurry\'p' and 'fn\'p'.
fn :: forall f xs b m.
      ( YulO2 (NP xs) b
      , UncurryNP'Fst f ~ xs
      , UncurryNP'Snd f ~ b
      , CurryNP (NP xs) b ~ f
      , YulCat'P (NP xs) ~ m
      , UncurryingNP f xs b m m m m Many
      , LiftFunction b m m Many ~ m b
      )
   => String                  -- ^ the function id
   -> LiftFunction f m m Many -- ^ the pure yul categorical value of @NP xs ↝ b@
   -> PureFn f                -- ^ a 'PureFn' of function type @f@
fn fid f = fn'p fid (uncurry'p @f f)

-- | Call a 'PureFn' by currying it with pure yul categorical values of @r ↝ xn@ until a pure yul categorical value of
-- @r ↝ b@ is returned.
--
-- On what a yul categorical value is, see $yul_cat_val.
call'p :: forall f xs b r m.
          ( YulO3 (NP xs) b r
          , UncurryNP'Fst f ~ xs
          , UncurryNP'Snd f ~ b
          , CurryNP (NP xs) b ~ f
          , YulCat'P r ~ m
          , CurryingNP xs b m m m Many
          , LiftFunction b m m Many ~ m b
          )
       => PureFn f                -- ^ a 'PureFn' of function type @f@
       -> LiftFunction f m m Many -- ^ a currying function type
call'p (MkFn f) = curryingNP @xs @b @m @m @m @Many
                  (\xs -> xs >.> jmpUserDefined (fnId f, fnCat f))
