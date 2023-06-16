{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE LinearTypes           #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}

module LoliYul.Core.Linear where

import           Data.Kind                    (Type)
import qualified Data.Text                    as T

import           Control.Category.Constrained (Cartesian (dis), Category (Obj),
                                               O2, O3, O4, type (⊗))
import           Control.Category.Linear      (P, copy, decode, discard, encode,
                                               ignore, merge, mkUnit, split)

import           LoliYul.Core.Types
import           LoliYul.Core.YulDSL


--------------------------------------------------------------------------------
-- Extra SMC Linear Combinators
--------------------------------------------------------------------------------

id1 :: forall k con r a.
       ( Cartesian k {-<-}, O2 k r a, con ~ Obj k {->-}
       , con (), (forall α β. (con α, con β) => con (α,β))
       ) => P k r a ⊸ P k r a
id1 a = a

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
passAp i f = copyAp' i id1 f

-- | The Linear function pipeline builder from left to right.
{-# INLINE (&) #-}
(&) :: forall a b m. a %m -> (a %m -> b) %m -> b
x & f = f x

--------------------------------------------------------------------------------
-- YulDSL Linear Combinators
--------------------------------------------------------------------------------

-- | Polymorphic port type for linear function APIs of YulDSL
type YulP r a = P YulDSL r a

yul_id :: forall a b r. (YulO3 r a b, YulSameBytes a b)
     => YulP r a ⊸ YulP r b
yul_id = encode YulId

-- TODO, use linear-base if'/bool
if1 :: forall r b. (YulObj r, YulObj b)
    => YulP r AbiBool
    ⊸  (forall r1. YulObj r1 => YulP r1 () ⊸ YulP r1 b)
    -> (forall r2. YulObj r2 => YulP r2 () ⊸ YulP r2 b)
    -> YulP r b
if1 b f g = encode (YulBool (decode f) (decode g)) b

-- TODO, use linear-base fst
fst1 :: forall a b r. YulO3 r a b
     => (YulP r a ⊗ YulP r b) ⊸ YulP r a
fst1 (a, b) = encode YulId (merge (a, discard b))

-- TODO use linear-ase
-- instance YulNum a => Num (YulP r a) where
(+:) :: forall a r. (YulO2 r a, YulNum a) => YulP r a ⊸ YulP r a ⊸ YulP r a
a +: b = encode YulNumAdd (merge (a,b))

negate1 :: forall a r. (YulO2 r a, YulNum a) => YulP r a ⊸ YulP r a
negate1 = encode YulNumNeg

(-:) :: forall a r. (YulO2 r a, YulNum a) =>  YulP r a ⊸ YulP r a ⊸ YulP r a
a -: b = encode YulNumAdd (merge (a, negate1 b))

yulConst :: forall a b r. YulO3 r a b
         => b -> (YulP r a ⊸ YulP r b)
yulConst b = encode (YulConst b)

-- abiPop :: forall a a' as' r. YulO4 r a a' as'
--        => YulP r a ⊸ YulP r (Maybe (a' :> as'))
-- abiPop = encode YulAbiPop

abiEncode :: forall a r. YulO2 r a
          => YulP r a ⊸ YulP r AbiBytes
abiEncode = encode YulAbiEnc

abiDecode :: forall a r. YulO2 r a
          => YulP r AbiBytes ⊸ YulP r (Maybe a)
abiDecode a = encode YulAbiDec a

sget :: forall v r. (YulObj r, YulVal v)
     => (YulP r AbiAddr ⊸ YulP r v)
sget = encode YulSGet

sput :: forall v r. (YulObj r, YulVal v)
     => YulP r AbiAddr ⊸ YulP r v ⊸ YulP r ()
sput toP valP = encode YulSPut (merge (toP, valP))
(<==) :: forall v r. (YulObj r, YulVal v)
      => YulP r AbiAddr ⊸ YulP r v ⊸ YulP r ()
(<==) = sput

sputAt :: forall v r. (YulObj r, YulVal v)
       => AbiAddr -> YulP r v ⊸ YulP r ()
sputAt to v = mkUnit v & \(v, u) -> yulConst to u & \a -> sput a v
(<=@) :: forall v r. (YulObj r, YulVal v)
      => AbiAddr -> YulP r v ⊸ YulP r ()
(<=@) = sputAt
infixr 1 <==, <=@

-- Port List

type family YulPortExplode (a :: Type) :: Type where
  YulPortExplode (YulP r ()) = ()
  YulPortExplode (YulP r (a :> as)) = YulPortExplode (YulP r a) :> YulPortExplode (YulP r as)
  YulPortExplode (YulP r a) = YulP r a

class YulObj a => YulPReducible a where
  yul_port_reduce :: forall r. YulObj r
                  => YulP r a ⊸ YulPortExplode (YulP r a)
  default yul_port_reduce :: (YulObj r, YulP r a ~ YulPortExplode (YulP r a))
                          => YulP r a ⊸ YulPortExplode (YulP r a)
  yul_port_reduce = id1

instance YulPReducible AbiAddr
instance YulPReducible AbiBool
instance YulPReducible AbiUInt
instance YulPReducible AbiInt
instance YulPReducible AbiBytes

instance forall a as. (YulPReducible a, YulPReducible as) => YulPReducible (a :> as) where
  yul_port_reduce p = yul_id @(a :> as) @(a ⊗ as) p & split &
                      \(a, as) -> yul_port_reduce a :> yul_port_reduce as

class YulO2 a as => YulPList a as where
  yul_port_pop :: forall r. YulObj r
               => YulP r (a :> as) ⊸ YulPortExplode (YulP r a) :> YulPortExplode (YulP r as)

instance forall a a' as'. (YulPReducible a, YulPList a' as') => YulPList a (a' :> as') where
  yul_port_pop p = yul_id @(a :> a' :> as') @(a ⊗ (a' :> as')) p & split &
                   \(a, as) -> yul_port_reduce a :> yul_port_pop as

instance forall a as. YulPList a as => YulPList (a :> as) () where
  yul_port_pop p = yul_id @((a :> as) :> ()) @((a :> as) ⊗ ()) p & split &
                   \(a, u) -> (ignore u a & yul_port_pop) :> ()

instance forall a. YulPReducible a => YulPList a () where
  yul_port_pop p = yul_id @(a :> ()) @(a ⊗ ()) p & split &
                   \(a, u) -> (ignore u a & yul_port_reduce) :> ()

defun :: forall a as b bs. (YulO4 a as b bs , YulPList a as)
      => T.Text
      -> (forall r1. YulObj r1 => YulPortExplode (YulP r1 (a :> as)) ⊸ YulP r1 (b :> bs))
      -> YulInternalFunction (a :> as) (b :> bs)
defun name f = YulInternFn name $ decode (\p -> f (yul_port_pop p))

-- exfun :: T.Text
--        -> (forall a as b bs r. YulO5 a as b bs r => YulP r (a :> as) ⊸ YulP r (b :> bs))
--        -> (forall r. YulObj r => YulP r AbiBytes ⊸ YulP r ())
--        -> YulExternFunc
-- exfun name f e = YulExternFn name (decode f) (decode e)
