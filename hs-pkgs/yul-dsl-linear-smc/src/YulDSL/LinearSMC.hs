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
import qualified Prelude                      as BasePrelude
-- linear-base
import           Prelude.Linear
import qualified Unsafe.Linear                as UnsafeLinear
-- linear-smc
import           Control.Category.Constrained (Cartesian, Category (Obj), O2, O3, O4, type (⊗))
import           Control.Category.Linear      (P, copy, decode, discard, encode, merge, mkUnit, split)
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

const'l :: forall a d r. YulO3 a d r
        => a -> (Yul'P r d ⊸ Yul'P r a)
const'l a = encode (YulEmbed a) . discard

coerce'l :: forall a b r. (YulO3 a b r, ABITypeCoercible a b)
         => (Yul'P r a ⊸ Yul'P r b)
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

-- | Define a `YulCat` morphism from a linear port function.
fn'l :: forall as b. YulO2 (NP as) b
     => String
     -> (forall r. YulO1 r => Yul'P r (NP as) ⊸ Yul'P r b)
     -> FnNP as b
fn'l fid f = MkFn fid $ decode f

-- class BuildableNP'P r x xs where
--   buildNP'p :: P YulCat r x ⊸  P YulCat r (NP xs) ⊸  P YulCat r (NP (x:xs))

-- fn'l fname f = MkFn fname $ decode (f . yul_port_reduce)

-- ap'lfn :: forall a b r. (YulPortReducible a, YulO3 a b r)
--        => Fn a b -> AtomizeNP (Yul'P r a) ⊸ Yul'P r b
-- ap'lfn fn a = encode (YulJump (fnId fn) (fnCat fn)) (yul_port_merge @a a)
