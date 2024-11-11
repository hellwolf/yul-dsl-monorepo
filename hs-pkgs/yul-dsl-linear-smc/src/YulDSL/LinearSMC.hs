{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE LinearTypes           #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

This module provides extra combinators to program 'YulDSL' in linear-types, in addition to "linear-base".

-}

module YulDSL.LinearSMC where

-- base
import           Data.Functor.Identity        (Identity)
import qualified Prelude                      as BasePrelude
-- linear-base
import           Prelude.Linear
import qualified Unsafe.Linear                as UnsafeLinear
-- linear-smc
import           Control.Category.Constrained (Cartesian, Category (Obj), O2, O3, O4, type (⊗))
import           Control.Category.Linear      (P, copy, decode, discard, encode, ignore, merge, mkUnit, split)
-- yul-dsl
import           YulDSL.Core
-- import Linear-SMC instances
import           YulDSL.LinearSMC.Categories  ()


------------------------------------------------------------------------------------------------------------------------
-- Extra Linear SMC Combinators
------------------------------------------------------------------------------------------------------------------------

copy' :: forall k con r a.
         ( Cartesian k {-<-}, O2 k r a, con ~ Obj k {->-}
         , con (), (forall α β. (con α, con β) => con (α,β))
         ) => P k r a ⊸ (P k r a, P k r a)
copy' a = copy a & split

copyAp :: forall k con r a b c.
          ( Cartesian k {-<-}, O4 k r a b c, con ~ Obj k {->-}
          , con (), (forall α β. (con α, con β) => con (α,β))
          ) => P k r a ⊸ (P k r a ⊸ P k r b) ⊸ (P k r a ⊸ P k r c) ⊸ P k r (b⊗c)
copyAp a f1 f2 = copy a & split & \(a1, a2) -> merge (f1 a1, f2 a2)

copyAp' :: forall k con r a b c.
           ( Cartesian k {-<-}, O4 k r a b c, con ~ Obj k {->-}
           , con (), (forall α β. (con α, con β) => con (α,β))
           ) => P k r a ⊸ (P k r a ⊸ P k r b) ⊸ (P k r a ⊸ P k r c) ⊸ (P k r b, P k r c)
copyAp' a f1 f2 = copy a & split & \(a1, a2) -> (f1 a1, f2 a2)

passAp :: forall k con r a b.
          ( Cartesian k {-<-}, O3 k r a b, con ~ Obj k {->-}
          , con (), (forall α β. (con α, con β) => con (α,β))
          ) => P k r a ⊸ (P k r a ⊸ P k r b) ⊸ (P k r a, P k r b)
passAp i = copyAp' i id

------------------------------------------------------------------------------------------------------------------------
-- linear-base instances
------------------------------------------------------------------------------------------------------------------------

instance FromInteger Integer where
  fromInteger = id

instance (KnownBool s, KnownNat n) => FromInteger (Maybe (INTx s n)) where
  fromInteger = UnsafeLinear.toLinear BasePrelude.fromInteger

instance (YulObj r, YulNum a) => Additive (YulCat r (Maybe a)) where
  a + b = YulNumAdd <.< YulProd a b <.< YulDup

instance (YulObj r, YulNum a) => AddIdentity (YulCat r (Maybe a)) where
  zero = YulEmbed (fromIntegral (0 :: Integer))

instance (YulObj r, YulNum a) => AdditiveGroup (YulCat r (Maybe a)) where
  negate a = YulNumNeg <.< a

------------------------------------------------------------------------------------------------------------------------
-- Yul Port Combinators
------------------------------------------------------------------------------------------------------------------------

{- Yul port Types -}

-- | Polymorphic port type for linear function APIs of YulDSL
type Yul'P r a = P YulCat r a

type UNIT'P  r = Yul'P r ()
type ADDR'P  r = Yul'P r ADDR
type BOOL'P  r = Yul'P r BOOL
type U256'P  r = Yul'P r U256
type I256'P  r = Yul'P r I256

type YulCat'P r a b = Yul'P r a ⊸ Yul'P r b

const'l :: forall a d r. YulO3 a d r
        => a -> (Yul'P r d ⊸ Yul'P r a)
const'l a = encode (YulEmbed a) . discard

coerce'l :: forall a b r. (YulO3 a b r, ABITypeCoercible a b)
         => Yul'P r a ⊸ Yul'P r b
coerce'l = encode YulCoerce

dup2'l :: forall a r. YulO2 a r
       => Yul'P r a ⊸ (Yul'P r a, Yul'P r a)
dup2'l = split . copy

{- YulNum utilities -}

instance (YulNum a, YulObj r) => Additive (Yul'P r (Maybe a)) where
  a + b = encode YulNumAdd (merge (a, b))

instance (YulNum a, YulObj r) => AddIdentity (Yul'P r (Maybe a)) where
  -- Note: uni-port is forbidden in linear-smc, but linear-base AdditiveGroup requires this instance.
  zero = error "unit not supported for Ports"

instance (YulNum a, YulObj r) => AdditiveGroup (Yul'P r (Maybe a)) where
  negate = encode YulNumNeg

instance (YulObj r, YulNum a) => MPOrd (Yul'P r a) (BOOL'P r) where
  a  <? b = encode (YulNumCmp (true , false, false)) (merge (a, b))
  a <=? b = encode (YulNumCmp (true , true , false)) (merge (a, b))
  a  >? b = encode (YulNumCmp (false, false, true )) (merge (a, b))
  a >=? b = encode (YulNumCmp (false, true , true )) (merge (a, b))
  a ==? b = encode (YulNumCmp (false, true , false)) (merge (a, b))
  a /=? b = encode (YulNumCmp (true , false, true )) (merge (a, b))

{- Control flow utilities -}

-- ifThenElse :: forall a r. YulO2 a r
--            => BoolP r ⊸ Yul'P r a ⊸ Yul'P r a ⊸ Yul'P r a

instance YulO2 a r => IfThenElse (BOOL'P r) (Yul'P r a) where
  ifThenElse c a b = encode YulITE (merge(c, merge(a, b)))

instance Consumable a => IfThenElse Bool a where
  ifThenElse True  a b = lseq b a
  ifThenElse False a b = lseq a b

--
-- Storage utilities
--

sget :: forall v r. (YulObj r, YulVal v)
     => Yul'P r ADDR ⊸ Yul'P r (Maybe v)
sget = encode YulSGet

sput :: forall v r. (YulObj r, YulVal v)
     => Yul'P r ADDR ⊸ Yul'P r v ⊸ Yul'P r ()
sput toP valP = encode YulSPut (merge (toP, valP))
(<==) :: forall v r. (YulObj r, YulVal v)
      => Yul'P r ADDR ⊸ Yul'P r v ⊸ Yul'P r ()
(<==) = sput

sputAt :: forall v r. (YulObj r, YulVal v)
       => ADDR -> Yul'P r v ⊸ Yul'P r ()
sputAt to v = mkUnit v & \(v', u) -> const'l to u & \a -> sput a v'
(<==@) :: forall v r. (YulObj r, YulVal v)
      => ADDR -> Yul'P r v ⊸ Yul'P r ()
(<==@) = sputAt
infixr 1 <==, <==@

{- Linear Function utilities -}

class UncurriableFn'L f as xs b where
  uncurryFn'l :: forall r f'.
                 ( YulO4 (NP as) (NP xs) b r
                 , f' ~ LiftFunction  f (P YulCat r) One
                 , xs ~ UncurryNP'Fst f
                 , b  ~ UncurryNP'Snd f
                 )
              => f'
              ⊸ YulCat'P r (NP as) (NP xs)
              -> YulCat'P r (NP as) b

instance forall as x.
         ( YulO2 (NP as) x
         , UncurryNP'Snd x ~ x
         , UncurryNP'Fst x ~ '[]
         , Identity x ~ LiftFunction x Identity One
         , UncurryNP'Snd (Identity x) ~ Identity x
         ) => UncurriableFn'L x as '[] x where
  uncurryFn'l :: forall r f'.
                 ( YulO1 r
                 , f' ~ LiftFunction x (P YulCat r) One
                 )
              => f'                          -- b
              ⊸  YulCat'P r (NP as) (NP '[]) -- g
              -> YulCat'P r (NP as) x
  -- putting a lot of type annotations since it's getting hard for brains.
  uncurryFn'l b g as = ignore (coerce'l (g as)) b'
    -- NOTE: Sorry GHC, I cannot convince you that we have evidences from the instance constraints for:
    -- Proof:
    --    f' ~ LiftFunction x (P YulCat r) One
    --       ~ P YulCat r (UncurryNP'Snd x)
    --       ~ P YulCat r x
    where b' = UnsafeLinear.coerce @_ @(Yul'P r x) (b :: f')

instance forall as x xs b g.
         ( YulO4 (NP as) x (NP xs) b
         , UncurriableFn'L g as xs b
         , UncurryNP'Fst g ~ xs
         , UncurryNP'Snd g ~ b
         ) => UncurriableFn'L (x -> g) as (x:xs) b where
  uncurryFn'l :: forall r f'.
                 ( YulO1 r
                 , f'     ~ LiftFunction  (x -> g) (P YulCat r) One
                 , (x:xs) ~ UncurryNP'Fst (x -> g)
                 , b      ~ UncurryNP'Snd (x -> g)
                 )
              => f'                             -- f
              ⊸  YulCat'P r (NP as) (NP (x:xs)) -- g
              -> YulCat'P r (NP as) b
  uncurryFn'l f g as =
    dup2'l as
    & \(as', as'') -> split (coerce'l @_ @(x, (NP xs)) (g as'))
    & \(x, xs) -> ignore (discard xs) (uncurryFn'l @g (f' x) g' as'')
    where
      -- NOTE: Sorry GHC, again, I can't convince you so I will coerce you.
      -- Proof:
      --   f' ~ LiftFunction  (x -> g) (P YulCat r) One
      --      ~ Yul'P r x ⊸ LiftFunction g (P YulCat r) One
      f' = UnsafeLinear.coerce @_ @(Yul'P r x ⊸ LiftFunction g (P YulCat r) One) (f :: f')
      g' :: YulCat'P r (NP as) (NP xs)
      g' as' = split (coerce'l @_ @(x, (NP xs)) (g as'))
               & \(x, xs) -> ignore (discard x) xs

curry'l :: forall f as b r f'.
        ( YulO3 (NP as) b r
        , as ~ UncurryNP'Fst f
        , b  ~ UncurryNP'Snd f
        , f' ~ LiftFunction f (P YulCat r) One
        , UncurriableFn'L f as as b
        )
        => f'
        -> YulCat'P r (NP as) b
curry'l f = uncurryFn'l @f f id

-- | Define a `YulCat` morphism from a linear port function.
fn'l :: forall as b. YulO2 (NP as) b
     => String
     -> (forall r. YulCat'P r (NP as) b)
     -> FnNP as b
fn'l fid cat'l = MkFn fid $ decode cat'l

-- class BuildableNP'P r x xs where
--   buildNP'p :: P YulCat r x ⊸  P YulCat r (NP xs) ⊸  P YulCat r (NP (x:xs))

-- fn'l fname f = MkFn fname $ decode (f . yul_port_reduce)

-- ap'lfn :: forall a b r. (YulPortReducible a, YulO3 a b r)
--        => Fn a b -> AtomizeNP (Yul'P r a) ⊸ Yul'P r b
-- ap'lfn fn a = encode (YulJump (fnId fn) (fnCat fn)) (yul_port_merge @a a)
