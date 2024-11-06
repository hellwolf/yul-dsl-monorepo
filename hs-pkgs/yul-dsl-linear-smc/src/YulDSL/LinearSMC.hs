{-# LANGUAGE LinearTypes           #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
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
import qualified Prelude                      as BasePrelude
import           Unsafe.Coerce                (unsafeCoerce)
-- linear-base
import           Prelude.Linear
import qualified Unsafe.Linear                as UnsafeLinear
-- linear-smc
import           Control.Category.Constrained (Cartesian (..), Category (Obj, (∘)), O2, O3, O4, type (⊗))
import           Control.Category.Linear      (P, copy, decode, discard, encode, merge, mkUnit, split)
-- yul-dsl
import           YulDSL.Core
-- import Linear-SMC instances
import           YulDSL.LinearSMC.Categories  ()

------------------------------------------------------------------------------------------------------------------------
-- Extra Linear SMC Combinators
------------------------------------------------------------------------------------------------------------------------

instance FromInteger Integer where
  fromInteger = id

instance (KnownBool s, KnownNat n) => FromInteger (Maybe (INTx s n)) where
  fromInteger = UnsafeLinear.toLinear BasePrelude.fromInteger

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
-- YulCat Combinators
------------------------------------------------------------------------------------------------------------------------

--
-- YulNum typeclass instances for the 'linear-base'.
--

instance (YulObj r, YulNum a) => Additive (YulCat r (Maybe a)) where
  a + b = YulNumAdd `YulComp` YulProd a b `YulComp` YulDup

instance (YulObj r, YulNum a) => AddIdentity (YulCat r (Maybe a)) where
  zero = YulEmbed (fromIntegral (0 :: Integer))

instance (YulObj r, YulNum a) => AdditiveGroup (YulCat r (Maybe a)) where
  negate a = YulNumNeg `YulComp` a

--
-- Value Functions utilities
--

-- uncurry (b)
instance forall a b m.
         ( m ~ YulCat a
         ) => UncurriableNP (YulCat a) (m b) '[] b where
  uncurryNP x _ = x

-- uncurry (x -> ...xs -> b)
instance forall a x xs b m g.
         ( m ~ YulCat a
         , YulO3 a x (NP xs)
         , UncurriableNP m g xs b
         ) => UncurriableNP (YulCat a) (m x -> g) (x:xs) b where
  uncurryNP f as = uncurryNP (f x) xs
    where ass = as >.> YulSplit
          x  = ass >.> YulExl
          xs = ass >.> YulExr

-- buildNP (x -> (x))
instance forall a x m.
         ( m ~ YulCat a
         , YulO2 a x
         ) => BuildableNP (YulCat a) x '[] where
  buildNP x _ = x >.> YulCoerce CoercibleSolo

instance forall a x x' xs'.
         ( YulO4 a x x' (NP xs')
         , BuildableNP (YulCat a) x' xs'
         ) => BuildableNP (YulCat a) x (x':xs') where
  buildNP x as = YulFork x (buildNP x' xs')
                 >.> YulCoerce CoercibleTupleAndNP
    where ass = as >.> YulSplit
          x'  = ass >.> YulExl
          xs' = ass >.> YulExr

-- | Define currying pure function of yul values.
fn :: forall as b m f.
      ( YulO2 (NP as) b
      , m ~ YulCat (NP as)
      , f ~ LiftFunction (CurryNP (NP as) b) m Many
      , UncurriableNP m f as b
      )
   => String -> f -> Fn (NP as) b
fn fid f = MkFn fid $ uncurryNP f (YulId @(NP as))

foo0 :: Fn (NP '[]) (Maybe U8)
foo0 = fn "id" (YulEmbed 42)

foo1 :: Fn (NP '[Maybe U8]) (Maybe U8)
foo1 = fn "id" (\a -> a + a)

foo2 :: Fn (NP '[Maybe U8, Maybe U8]) (Maybe U8)
foo2 = fn "id" (\a b -> a + b)

foo3 :: Fn (NP '[Maybe U8, Maybe U8, Maybe U8]) (Maybe U8)
foo3 = fn "id" (\a b c -> a + b + c)

foo4 :: Fn (NP '[Maybe U8, Maybe U8, Maybe U8, Maybe U8]) (Maybe U8)
foo4 = fn "id" (\a b c d -> a + b + c + d)

-- ap'vfn :: forall a b r. (YulCatReducible a, YulO3 a b r)
--       => Fn a b -> AtomizeNP (YulCat r a) -> YulCat r b

-- ap'vfn fn a = YulJump (fnId fn) (fnCat fn) `YulComp` yul_cat_merge @a a

------------------------------------------------------------------------------------------------------------------------
-- Yul Port Combinators
------------------------------------------------------------------------------------------------------------------------

--
-- Yul port Types
--

-- | Polymorphic port type for linear function APIs of YulDSL
type YulP r a = P YulCat r a

type UNIT'p  r = YulP r ()
type ADDR'p  r = YulP r ADDR
type BOOL'p  r = YulP r BOOL
type U256'p  r = YulP r U256
type I256'p  r = YulP r I256

yulConst :: forall a d r. YulO3 a d r
         => a -> (YulP r d ⊸ YulP r a)
yulConst a = encode (YulEmbed a) . discard

-- coerceP :: forall a b r. (YulO3 r a b, YulCoercible a b)
--         => YulP r a ⊸ YulP r b
-- coerceP = encode YulCoerce

--
-- YulNum utilities
--

instance (YulNum a, YulObj r) => Additive (YulP r (Maybe a)) where
  a + b = encode YulNumAdd (merge (a, b))

instance (YulNum a, YulObj r) => AddIdentity (YulP r (Maybe a)) where
  -- Note: uni-port is forbidden in linear-smc, but linear-base AdditiveGroup requires this instance.
  zero = error "unit not supported for Ports"

instance (YulNum a, YulObj r) => AdditiveGroup (YulP r (Maybe a)) where
  negate = encode YulNumNeg
dup2P :: YulO2 a r => YulP r a ⊸ (YulP r a, YulP r a)
dup2P = split . copy

instance (YulObj r, YulNum a) => MPOrd (YulP r a) (BOOL'p r) where
  a  <? b = encode (YulNumCmp (true , false, false)) (merge (a, b))
  a <=? b = encode (YulNumCmp (true , true , false)) (merge (a, b))
  a  >? b = encode (YulNumCmp (false, false, true )) (merge (a, b))
  a >=? b = encode (YulNumCmp (false, true , true )) (merge (a, b))
  a ==? b = encode (YulNumCmp (false, true , false)) (merge (a, b))
  a /=? b = encode (YulNumCmp (true , false, true )) (merge (a, b))

-- Control flow utilities

-- ifThenElse :: forall a r. YulO2 a r
--            => BoolP r ⊸ YulP r a ⊸ YulP r a ⊸ YulP r a

instance YulO2 a r => IfThenElse (BOOL'p r) (YulP r a) where
  ifThenElse c a b = encode YulITE (merge(c, merge(a, b)))

instance Consumable a => IfThenElse Bool a where
  ifThenElse True  a b = lseq b a
  ifThenElse False a b = lseq a b

--
-- Storage utilities
--

sget :: forall v r. (YulObj r, YulVal v)
     => YulP r ADDR ⊸ YulP r (Maybe v)
sget = encode YulSGet

sput :: forall v r. (YulObj r, YulVal v)
     => YulP r ADDR ⊸ YulP r v ⊸ YulP r ()
sput toP valP = encode YulSPut (merge (toP, valP))
(<==) :: forall v r. (YulObj r, YulVal v)
      => YulP r ADDR ⊸ YulP r v ⊸ YulP r ()
(<==) = sput

sputAt :: forall v r. (YulObj r, YulVal v)
       => ADDR -> YulP r v ⊸ YulP r ()
sputAt to v = mkUnit v & \(v', u) -> yulConst to u & \a -> sput a v'
(<==@) :: forall v r. (YulObj r, YulVal v)
      => ADDR -> YulP r v ⊸ YulP r ()
(<==@) = sputAt
infixr 1 <==, <==@

--
-- Linear Function utilities
--

instance forall r b m.
         ( m ~ P YulCat r
         ) => UncurriableNP (P YulCat r) (m b) '[] b where
  uncurryNP x _ = x

-- -- uncurry (x -> ...xs -> b)
-- instance forall a x xs b m g.
--          ( m ~ YulCat a
--          , YulO3 a x (NP xs)
--          , UncurriableNP m g xs b
--          ) => UncurriableNP (YulCat a) (m x -> g) (x:xs) b where
--   uncurryNP f as = uncurryNP (f x) xs
--     where ass = as >.> YulSplit
--           x  = ass >.> YulExl
--           xs = ass >.> YulExr

-- | Define a `YulCat` morphism from a linear port function.
fn'l :: forall a b. (YulO2 a b)
    => String
    -> (forall r. YulObj r => LiftFunction (CurryNP a b) (P YulCat r) One)
    -> Fn a b
fn'l = _

-- fn'l fname f = MkFn fname $ decode (f . yul_port_reduce)

-- ap'lfn :: forall a b r. (YulPortReducible a, YulO3 a b r)
--        => Fn a b -> AtomizeNP (YulP r a) ⊸ YulP r b
-- ap'lfn fn a = encode (YulJump (fnId fn) (fnCat fn)) (yul_port_merge @a a)
