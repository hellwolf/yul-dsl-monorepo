{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
module YulDSL.Effects.LinearSMC.LinearFn
  ( -- * Build Linear Yul Functions
    CreateLinearFn (lfn)
    -- * Call Yul Functions Linearly
  , LinearlyCallableFn, CallFnLinearly (callFn'l)
  , callFn'lpp
    -- * Call external smart contract functions
  , externalCall'l
  ) where
-- base
import GHC.TypeLits                          (type (+))
-- linear-base
import Control.Category.Linear               (merge, mkUnit)
import Prelude.Linear
import Unsafe.Linear                         qualified as UnsafeLinear
-- yul-dsl
import YulDSL.Core
--
import YulDSL.Effects.LinearSMC.LinearYulCat
import YulDSL.Effects.LinearSMC.YulPort


------------------------------------------------------------------------------------------------------------------------
-- lfn
------------------------------------------------------------------------------------------------------------------------

-- | Create linear kind of yul functions.
class CreateLinearFn (iEff :: PortEffect) (oEff :: PortEffect) (fnEff :: LinearEffectKind)
      | iEff oEff -> fnEff where
  -- | Define a `YulCat` morphism from a yul port diagram.
  lfn :: forall f xs b.
    ( YulO2 (NP xs) b
    -- constraint f, using b xs
    , EquivalentNPOfFunction f xs b
    )
    => String
    -> (forall r. YulO1 r => P'x iEff r (NP xs) ⊸ P'x oEff r b)
    -> Fn fnEff (CurryNP (NP xs) b)

instance forall vd. CreateLinearFn (VersionedPort 0) (VersionedPort vd) (VersionedInputOutput vd) where
  lfn cid f = MkFn (cid, decode'lvv f)

instance forall vd. CreateLinearFn PurePort (VersionedPort vd) (PureInputVersionedOutput vd) where
  lfn cid f = MkFn (cid, decode'lpv f)

------------------------------------------------------------------------------------------------------------------------
-- callFn'l
------------------------------------------------------------------------------------------------------------------------

-- This makes the signature of CallFnLinearly easier to repeat.
type LinearlyCallableFn f x xs b r v1 vd vn =
    ( YulO4 x (NP xs) b r
    , v1 + vd ~ vn
    -- constraint f
    , EquivalentNPOfFunction f (x:xs) b
    -- constraint b
    , LiftFunction b (YulCat'LVV v1 v1 r ()) (P'V vn r) One ~ P'V vn r b
    -- CurryingNP instance on "NP xs -> b"
    , CurryingNP xs b (P'V v1 r) (P'V vn r) (YulCat'LVV v1 v1 r ()) One
    )

-- | Calling @fnEff@ kind of yul function will increase data version by @vd@.
class CallFnLinearly fnEff vd | fnEff -> vd where
  -- | Call functions with versioned yul port and get versioned yul port.
  callFn'l :: forall f x xs b r v1 vn.
    LinearlyCallableFn f x xs b r v1 vd vn
    => Fn fnEff f
    -> (P'V v1 r x ⊸ LiftFunction (CurryNP (NP xs) b) (P'V v1 r) (P'V vn r) One)
  -- ^ All other function kinds is coerced into calling as if it is a versioned input output.
  callFn'l f = callFn'l @(VersionedInputOutput vd) @vd @f
               (UnsafeLinear.coerce f)

instance CallFnLinearly (VersionedInputOutput vd) vd where
  callFn'l :: forall f x xs b r v1 vn.
    LinearlyCallableFn f x xs b r v1 vd vn
    => Fn (VersionedInputOutput vd) f
    -> (P'V v1 r x ⊸ LiftFunction (CurryNP (NP xs) b) (P'V v1 r) (P'V vn r) One)
  callFn'l (MkFn t) x =
    mkUnit x
    & \(x', u) -> curryingNP @xs @b @(P'V v1 r) @(P'V vn r) @(YulCat'LVV v1 v1 r ()) @One
    $ \(MkYulCat'LVV fxs) -> encode'lvv (YulJmpU t)
    $ cons'l x' (fxs u)

instance CallFnLinearly (PureInputVersionedOutput vd) vd
instance CallFnLinearly Pure 0
instance CallFnLinearly Total 0

-- | Call pure function with pure yul port and get pure yul port.
callFn'lpp :: forall f x xs b r eff.
  ( YulO4 x (NP xs) b r
  -- constraint f
  , EquivalentNPOfFunction f (x:xs) b
  -- constraint b
  , LiftFunction b (YulCat'LPP r ()) (P'P r) One ~ P'P r b
  -- CurryingNP instance on "NP xs -> b"
  , CurryingNP xs b (P'P r) (P'P r) (YulCat'LPP r ()) One
  )
  => Fn (eff :: PureEffectKind) f
  -> (P'P r x ⊸ LiftFunction (CurryNP (NP xs) b) (P'P r) (P'P r) One)
callFn'lpp (MkFn t) x =
  mkUnit x
  & \(x', u) -> curryingNP @_ @_ @(P'P r) @(P'P r) @(YulCat'LPP r ()) @One
  $ \(MkYulCat'LPP fxs) -> encode'lpp (YulJmpU t)
  $ cons'l x' (fxs u)

------------------------------------------------------------------------------------------------------------------------
-- calling external functions
------------------------------------------------------------------------------------------------------------------------

externalCall'l :: forall f x xs b r v1 addrEff.
  LinearlyCallableFn f x xs b r v1 1 (v1 + 1)
  => ExternalFn f
  -> P'x addrEff r ADDR
  ⊸ (P'V v1 r x ⊸ LiftFunction (CurryNP (NP xs) b) (P'V v1 r) (P'V (v1 + 1) r) One)
externalCall'l (MkExternalFn sel) addr x =
  mkUnit x
  & \(x', u) -> curryingNP @_ @_ @(P'V v1 r) @(P'V (v1 + 1) r) @(YulCat'LVV v1 v1 r ()) @One
  $ \(MkYulCat'LVV fxs) -> encode'lvv YulId
  $ go (cons'l x' (fxs u))
  where go :: forall. P'x (VersionedPort v1) r (NP (x : xs)) ⊸ P'V v1 r b
        go args = let !(args', u) = mkUnit args
                  in encode'lvv @_ @_ @_ @v1 @0 (YulCall sel)
                     (merge (merge (UnsafeLinear.coerce addr, emb'l 0 u), args'))
