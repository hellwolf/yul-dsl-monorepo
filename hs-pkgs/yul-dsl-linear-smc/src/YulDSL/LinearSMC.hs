{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
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
import           Data.Functor.Identity        (Identity)
import           Data.Typeable                (Typeable)
import qualified Prelude                      as BasePrelude
import           Unsafe.Coerce                (unsafeCoerce)
-- linear-base
import           Prelude.Linear
import qualified Unsafe.Linear                as UnsafeLinear
-- linear-smc
import           Control.Category.Constrained (Cartesian (..), Category (Obj), O2, O3, O4, type (⊗))
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

instance (Typeable s, KnownNat n) => FromInteger (INTx s n) where
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
passAp i f = copyAp' i id f

------------------------------------------------------------------------------------------------------------------------
-- YulCat Combinators
------------------------------------------------------------------------------------------------------------------------

--
-- YulCat Type Arithmetic
--

-- | Reduce and merge inhabitants of yul port types using `YulPortReduce`.
class YulObj a => YulCatReducible a where
  -- | Reduce single-complex port to multiple ports.
  yul_cat_reduce :: forall r. YulObj r => YulCat r a -> AtomizeNP (YulCat r a)
  -- | Default instance for irreducible yul ports as base cases.
  default yul_cat_reduce :: (YulObj r, YulCat r a ~ AtomizeNP (YulCat r a)) => YulCat r a -> AtomizeNP (YulCat r a)
  yul_cat_reduce = id

-- Irreducible yul cat:
instance YulCatReducible () where
instance YulCatReducible ADDR
instance YulCatReducible BOOL
instance (Typeable s, KnownNat n) => YulCatReducible (INTx s n)
instance YulCatReducible BYTES

instance forall a as. (YulCatReducible a, YulCatReducible as) => YulCatReducible (a :* as) where
  yul_cat_reduce c = yul_cat_reduce @a (exl `YulComp` s) :*
                     yul_cat_reduce @as (exr `YulComp` s)
    where s = unsafeCoerce -- this feels so dirty
              @(YulCat (a :* as) ( UnM (HeadANP (AtomizeNP (Identity (a :* as))))
                                 , UnM (TailANP (AtomizeNP (Identity (a :* as))))))
              @(YulCat (a :* as) (a ⊗ as))
              (YulSplit @(a :* as)) `YulComp` c

--
-- YulNum typeclass instances for the 'linear-base'.
--

instance (YulObj r, YulNum a) => Additive (YulCat r a) where
  a + b = YulNumAdd `YulComp` YulProd a b `YulComp` YulDup

instance (YulObj r, YulNum a) => AddIdentity (YulCat r a) where
  zero = YulEmbed (fromIntegral (0 :: Integer))

instance (YulObj r, YulNum a) => AdditiveGroup (YulCat r a) where
  negate a = YulNumNeg `YulComp` a

--
-- Value Functions utilities
--

vfn :: forall a b p. (YulO2 a b, YulCatReducible a)
    => (AtomizeNP (YulCat a a) -> YulCat a b)
    -> YulCat a b
vfn fn = fn (yul_cat_reduce @a YulId)

ap'vfn :: forall a b p r. (YulCatReducible a, YulO3 a b r)
      => Fn a b -> AtomizeNP (YulCat r a) ⊸ YulCat r b
ap'vfn = undefined

------------------------------------------------------------------------------------------------------------------------
-- Yul Port Combinators
------------------------------------------------------------------------------------------------------------------------

--
-- Yul port Types
--

-- | Polymorphic port type for linear function APIs of YulDSL
type YulP r a = P YulCat r a

type UnitP    r = YulP r ()
type AddrP    r = YulP r ADDR
type BoolP    r = YulP r BOOL
type Uint256P r = YulP r UINT256
type Int256P  r = YulP r INT256
type BytesP   r = YulP r BYTES

yulConst :: forall a b r. YulO3 r a b
         => a -> (YulP r b ⊸ YulP r a)
yulConst a = \b -> encode (YulEmbed a) (discard b)

coerceP :: forall a b r. (YulO3 r a b, YulCoercible a b)
        => YulP r a ⊸ YulP r b
coerceP = encode YulCoerce

--
-- YulNum utilities
--

instance (YulNum a, YulObj r) => Additive (YulP r a) where
  a + b = encode YulNumAdd (merge (a, b))

instance (YulNum a, YulObj r) => AddIdentity (YulP r a) where
  -- Note: uni-port is forbidden in linear-smc, but linear-base AdditiveGroup requires this instance.
  zero = error "unit not supported for Ports"

instance (YulNum a, YulObj r) => AdditiveGroup (YulP r a) where
  negate = encode YulNumNeg
dup2P :: YulO2 a r => YulP r a ⊸ (YulP r a, YulP r a)
dup2P = split . copy

instance (YulObj r, YulNum a) => MPOrd (YulP r a) (BoolP r) where
  a  <? b = encode (YulNumCmp (true , false, false)) (merge (a, b))
  a <=? b = encode (YulNumCmp (true , true , false)) (merge (a, b))
  a  >? b = encode (YulNumCmp (false, false, true )) (merge (a, b))
  a >=? b = encode (YulNumCmp (false, true , true )) (merge (a, b))
  a ==? b = encode (YulNumCmp (false, true , false)) (merge (a, b))
  a /=? b = encode (YulNumCmp (true , false, true )) (merge (a, b))

-- Control flow utilities

-- ifThenElse :: forall a r. YulO2 a r
--            => BoolP r ⊸ YulP r a ⊸ YulP r a ⊸ YulP r a

instance YulO2 a r => IfThenElse (YulP r BOOL) (YulP r a) where
  ifThenElse i a b = encode YulITE (merge (i, merge(a, b)))

--
-- Storage utilities
--

sget :: forall v r. (YulObj r, YulVal v)
     => YulP r ADDR ⊸ YulP r  v
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
-- Port List Type Arithmetic
--

-- | Reduce and merge inhabitants of yul port types using `YulPortReduce`.
class YulObj a => YulPortReducible a where
  -- | Reduce single-complex port to multiple ports.
  yul_port_reduce :: forall r. YulObj r => YulP r a ⊸ AtomizeNP (YulP r a)
  -- | Default instance for irreducible yul ports as base cases.
  default yul_port_reduce :: (YulObj r, YulP r a ~ AtomizeNP (YulP r a)) => YulP r a ⊸ AtomizeNP (YulP r a)
  yul_port_reduce = id

  -- | Merge multiple orts to a single-complex port.
  yul_port_merge :: forall r. YulObj r => AtomizeNP (YulP r a) ⊸ YulP r a
  -- | Default instance for irreducible yul ports as base cases.
  default yul_port_merge :: (YulObj r, YulP r a ~ AtomizeNP (YulP r a)) => AtomizeNP (YulP r a) ⊸ YulP r a
  yul_port_merge = id

-- Irreducible yul ports:
instance YulPortReducible () where
instance YulPortReducible ADDR
instance YulPortReducible BOOL
instance (Typeable s, KnownNat n) => YulPortReducible (INTx s n)
instance YulPortReducible BYTES

instance forall a as. (YulPortReducible a, YulPortReducible as) => YulPortReducible (a :* as) where
  yul_port_reduce p = coerceP @(a :* as) @(a ⊗ as) p & split &
                      \(a, as) -> yul_port_reduce a :* yul_port_reduce as
  yul_port_merge (a :* as) = merge (yul_port_merge a, yul_port_merge as) &
                             coerceP @(a ⊗ as) @(a :* as)

--
-- Linear Function utilities
--

-- | Define a `YulCat` morphism from a linear port function.
lfn :: forall a b p. (YulO2 a b, YulPortReducible a)
    => (forall r. YulObj r => AtomizeNP (YulP r a) ⊸ YulP r b)
    -> YulCat a b
lfn f = decode (f . yul_port_reduce)

ap'lfn :: forall a b p r. (YulPortReducible a, YulO3 a b r)
      => Fn a b -> AtomizeNP (YulP r a) ⊸ YulP r b
ap'lfn (LibraryFn fname _) a = encode (YulJump fname) (yul_port_merge @a a)
-- apFn (ExternalFn _ _ _)  a = coerce a
