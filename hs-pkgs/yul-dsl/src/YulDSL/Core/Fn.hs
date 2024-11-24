{-# LANGUAGE AllowAmbiguousTypes #-}
{-|

Copyright   : (c) 2023-2024 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

-}
module YulDSL.Core.Fn
  ( FnCat (MkFnCat), FnNP, Fn (MkFn, unFn), fnId, fnCat
  , PureFn
  , AnyFn (MkAnyFn)
  , uncurry'p, fn'p, fn
  , call'p
  ) where

-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCat


{- * FnCat: yul function type and its aliases -}

-- | Yul functions are encoded in their categorical forms.
data FnCat eff a b where
  -- | Create a yul function from its unique id and morphism from @a@ to @b@.
  MkFnCat :: forall eff a b. YulO2 a b
          => { fnId  :: String         -- ^ the unique id of the yul function
             , fnCat :: YulCat eff a b -- ^ the morphism of the yul function
             }
          -> FnCat eff a b

instance YulO2 a b => Show (FnCat eff a b) where
  show (MkFnCat fid cat) = "fn " ++ fid ++ ":\n" ++ show cat

-- | Yul functions that have their arguments in 'NP' forms.
type FnNP eff xs b = FnCat eff (NP xs) b

-- | Yul functions that denoted in currying function forms.
--
--   Note: Fn (a1 -> a2 -> ...aN -> b) ~ FnNP (NP [a1,a2...aN]) b
newtype Fn eff f = MkFn { unFn :: FnNP eff (UncurryNP'Fst f) (UncurryNP'Snd f) }

{- * AnyFn -}

-- | Existential type for any @FnCat a b@.
data AnyFn = forall eff a b. YulO2 a b => MkAnyFn (FnCat eff a b)

deriving instance Show AnyFn

{- * PureFn: yul functions with pure effect -}

-- | Function without side effects, hence pure.
type PureFn = Fn MkPure

-- $yul_cat_val
--
-- A yul categorical value of @a ↝ b@ is another way of saying a morphism from @a@ to @b@ in the category of 'YulCat'.
--
-- One may also wrap it around a effect kind, e.g. @Pure (a ↝ b)@ means a pure yul categorical value of @a ↝ b@.

-- | Uncurry a currying function @f@ with pure yul categorical values of @NP xs ↝ x_n@ that returns a pure categorical
-- value @NP xs ↝ b@.
--
-- = Note: How to Read This Type Signature
-- When given:
--
--   * NP xs = (x1, x2 ... xn)
--   * x1' = Pure (NP xs ↝ x1), x2' ... etc.
--   * f = λ x1' -> λ x2' -> ... λ xn' -> Pure (NP xs ↝ b)
--
-- It returns: Pure (NP xs ↝ b)
--
-- On what a yul categorical value is, see $yul_cat_val.
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
             , YulCat MkPure (NP xs) ~ m
             , UncurryingNP f xs b m m m m Many
             )
          => LiftFunction f m m Many -- ^ uncurrying function type
          -> YulCat MkPure (NP xs) b -- ^ result type, or its short form @m b@
uncurry'p f = uncurryingNP @f @xs @b @m @m @m @m f YulId

-- | Create a `PureFn` given a function id and a pure categorical value @NP xs ↝ b@
--
-- On what a yul categorical value is, see $yul_cat_val.
fn'p :: forall f xs b.
        ( YulO2 (NP xs) b
        , CurryNP (NP xs) b ~ f
        , UncurryNP'Fst f ~ xs
        , UncurryNP'Snd f ~ b
        )
     => String                     -- ^ the function id
     -> YulCat MkPure (NP xs) b    -- ^ the pure yul categorical value of @NP xs ↝ b@
     -> PureFn (CurryNP (NP xs) b) -- ^ a 'PureFn' of function type @f@
fn'p fid cat'l = MkFn (MkFnCat fid cat'l)

-- | An alias for creating 'PureFn' using 'uncurry\'p' and 'fn\'p'.
fn :: forall f xs b m.
      ( YulO2 (NP xs) b
      , UncurryNP'Fst f ~ xs
      , UncurryNP'Snd f ~ b
      , CurryNP (NP xs) b ~ f
      , YulCat MkPure (NP xs) ~ m
      , UncurryingNP f xs b m m m m Many
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
          , YulCat MkPure r ~ m
          , CurryingNP xs b m m m Many
          )
       => PureFn f                -- ^ a 'PureFn' of function type @f@
       -> LiftFunction f m m Many -- ^ a currying function type
call'p (MkFn f) = curryingNP @xs @b @m @m @m @Many
                  (\xs -> xs >.> YulJump (fnId f) (fnCat f))
