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

module YulDSL.Linear where

-- base
import           Data.Kind                    (Type)
import           Data.Typeable                (Typeable)
import           GHC.TypeNats                 (KnownNat)
import           Prelude.Linear
-- linear-smc
import           Control.Category.Constrained (Cartesian, Category (Obj), O2, O3, O4, type (⊗))
import           Control.Category.Linear      (P, copy, decode, discard, encode, merge, mkUnit, split)
-- yul-dsl
import           YulDSL.Core
--
import           YulDSL.Linear.Categories     ()

------------------------------------------------------------------------------------------------------------------------
-- Extra SMC Linear Combinators
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
passAp i f = copyAp' i id f

------------------------------------------------------------------------------------------------------------------------
-- YulDSL Linear Combinators
------------------------------------------------------------------------------------------------------------------------

-- | Polymorphic port type for linear function APIs of YulDSL
type YulP r a = P YulCat r a

-- Port Types

type UnitP    r = YulP r ()
type AddrP    r = YulP r ADDR
type BoolP    r = YulP r BOOL
type Uint256P r = YulP r UINT256
type Int256P  r = YulP r INT256
type BytesP   r = YulP r BYTES

dup2P :: YulO2 a r => YulP r a ⊸ (YulP r a, YulP r a)
dup2P = split . copy

instance (YulNum a, YulObj r) => Additive (YulP r a) where
  a + b = encode YulNumAdd (merge (a, b))

instance (YulNum a, YulObj r) => AddIdentity (YulP r a) where
  zero = error "unit not supported for Ports"

instance (YulNum a, YulObj r) => AdditiveGroup (YulP r a) where
  negate = encode YulNumNeg
  a - b = encode YulNumAdd (merge (a, negate b))

-- Vars

-- newtype Var r a = MkVar { getVar :: UnitP r ⊸ YulP r a }
--
-- mkVar :: forall a r. YulO2 a r => YulP r a ⊸ Var r a
-- mkVar a = MkVar (\u -> ignore u a)
--
-- -- useVar :: forall a r. YulO2 a r => Var r a
--
-- --
-- -- unVar :: forall a r. YulO2 a r => Var a -> UnitP r ⊸ YulP r a
-- -- unVar a = encode a
--
-- instance (YulObj r, YulNum a) => Additive (Var r a) where
--   a + b = (\u -> copy u & split & \(u1, u2) -> a u1 + b u2)
--
-- instance (YulObj r, YulNum a) => AddIdentity (Var r a) where
--   zero = (\u -> yulConst 0 u)
--
-- instance (YulObj r, YulNum a) => AdditiveGroup (Var r a) where
--   a - b = (\u -> copy u & split & \(u1, u2) -> a u1 - b u2)

-- Utilities

yulCoerce :: forall a b r. (YulO3 r a b, YulCoercible a b)
     => YulP r a ⊸ YulP r b
yulCoerce = encode YulCoerce

-- Control Flow

yulConst :: forall a b r. YulO3 r a b
         => a -> (YulP r b ⊸ YulP r a)
yulConst a = \b -> encode (YulEmbed a) (discard b)

ifThenElse :: forall a r. YulO2 a r
           => BoolP r ⊸ YulP r a ⊸ YulP r a ⊸ YulP r a
ifThenElse i a b = encode YulITE (merge (i, merge(a, b)))

-- YulVal utilities

(<?) :: forall a r. (YulNum a, YulObj r) => YulP r a ⊸ YulP r a ⊸ BoolP r
a <? b = encode (YulNumCmp (true, false, false)) (merge (a, b))
(<=?) :: forall a r. (YulNum a, YulObj r) => YulP r a ⊸ YulP r a ⊸ BoolP r
a <=? b = encode (YulNumCmp (true, true, false)) (merge (a, b))
(>?) :: forall a r. (YulNum a, YulObj r) => YulP r a ⊸ YulP r a ⊸ BoolP r
a >? b = encode (YulNumCmp (false, false, true)) (merge (a, b))
(>=?) :: forall a r. (YulNum a, YulObj r) => YulP r a ⊸ YulP r a ⊸ BoolP r
a >=? b = encode (YulNumCmp (false, true, true)) (merge (a, b))
(==?) :: forall a r. (YulNum a, YulObj r) => YulP r a ⊸ YulP r a ⊸ BoolP r
a ==? b = encode (YulNumCmp (false, true, false)) (merge (a, b))
(/=?) :: forall a r. (YulNum a, YulObj r) => YulP r a ⊸ YulP r a ⊸ BoolP r
a /=? b = encode (YulNumCmp (true, false, true)) (merge (a, b))
infixr 4 <?, <=?, >?, >=?, ==?, /=?

-- Storage

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

-- Port List Type Arithmetic

-- | Reduce single-complex-port type to multiple-ports type.
--
--   Note: closed type family is used so that overlapping instances are allowed.
type family YulPortReduce (a :: Type) :: Type where
  YulPortReduce (YulP r (a ⊗ b))  = YulPortReduce (YulP r a) :> YulPortReduce (YulP r b)
  YulPortReduce (YulP r (a :> b)) = YulPortReduce (YulP r a) :> YulPortReduce (YulP r b)
  YulPortReduce (YulP r [a])      = [YulPortReduce (YulP r a)]
  YulPortReduce (YulP r a)        = YulP r a

-- | Reduce and merge inhabitants of yul port types using `YulPortReduce`.
class YulObj a => YulPortReducible a where
  -- | Reduce single-complex port to multiple ports.
  yul_port_reduce :: forall r. YulObj r
                  => YulP r a ⊸ YulPortReduce (YulP r a)
  -- | Default instance for irreducible yul ports as base cases.
  default yul_port_reduce :: (YulObj r, YulP r a ~ YulPortReduce (YulP r a))
                          => YulP r a ⊸ YulPortReduce (YulP r a)
  yul_port_reduce = id

  -- | Merge multiple orts to a single-complex port.
  yul_port_merge :: forall r. YulObj r
                 => YulPortReduce (YulP r a) ⊸ YulP r a
  -- | Default instance for irreducible yul ports as base cases.
  default yul_port_merge :: (YulObj r, YulP r a ~ YulPortReduce (YulP r a))
                         => YulPortReduce (YulP r a) ⊸ YulP r a
  yul_port_merge = id

-- Irreducible yul ports:

instance YulPortReducible () where
instance YulPortReducible ADDR
instance YulPortReducible BOOL
instance (Typeable s, KnownNat n) => YulPortReducible (INTx s n)
instance YulPortReducible BYTES

instance forall a as. (YulPortReducible a, YulPortReducible as) => YulPortReducible (a :> as) where
  yul_port_reduce p = yulCoerce @(a :> as) @(a ⊗ as) p & split &
                      \(a, as) -> yul_port_reduce a :> yul_port_reduce as
  yul_port_merge (a :> as) = merge (yul_port_merge a, yul_port_merge as) & yulCoerce @(a ⊗ as) @(a :> as)

defun' :: forall a b p. (YulO2 a b, YulPortReducible a)
      => (forall r. YulObj r => YulPortReduce (YulP r a) ⊸ YulP r b)
      -> YulCat a b
defun' f = decode (f . yul_port_reduce)

lfn :: forall a b p. (YulO2 a b, YulPortReducible a)
      => (forall r. YulObj r => YulPortReduce (YulP r a) ⊸ YulP r b)
      -> YulCat a b
lfn f = decode (f . yul_port_reduce)

apfun :: forall a b p r. (YulPortReducible a, YulO3 a b r)
      => Fn a b -> YulPortReduce (YulP r a) ⊸ YulP r b
apfun (LibraryFn fname _) a = encode (YulJump fname) (yul_port_merge @a a)
-- apfun (ExternalFn _ _ _) _  = error "FIXME callFn external function"
