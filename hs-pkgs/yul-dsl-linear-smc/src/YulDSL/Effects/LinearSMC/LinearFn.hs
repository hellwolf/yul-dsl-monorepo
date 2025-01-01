{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
module YulDSL.Effects.LinearSMC.LinearFn
  ( -- * Build LinearFn And Call SomeFn Linearly
    lfn, callLfn'l, callFn'l
  ) where
-- base
import GHC.TypeLits                          (type (+))
-- linear-base
import Control.Category.Linear               (discard)
import Prelude.Linear
import Unsafe.Linear                         qualified as UnsafeLinear
-- yul-dsl
import YulDSL.Core
--
import YulDSL.Effects.LinearSMC.LinearYulCat
import YulDSL.Effects.LinearSMC.YulPort


-- | Create linear function kind.
class CreateLinearFn (iEff :: PortEffect) (oEff :: PortEffect) (fnEff :: LinearEffect) | iEff oEff -> fnEff where
  -- | Define a `YulCat` morphism from a yul port diagram.
  lfn :: forall f xs b.
    ( YulO2 (NP xs) b
    -- constraint f, using b xs
    , CurryNP (NP xs) b ~ f
    , UncurryNP'Fst f ~ xs
    , UncurryNP'Snd f ~ b
    )
    => String
    -> (forall r. YulO1 r => P'x iEff r (NP xs) ⊸ P'x oEff r b)
    -> Fn fnEff (CurryNP (NP xs) b)

instance forall vd. CreateLinearFn (VersionedPort 0) (VersionedPort vd) (VersionedInputOutput vd) where
  lfn fid f = MkFn (fid, decode'lvv f)

instance forall vd. CreateLinearFn PurePort (VersionedPort vd) (PureInputVersionedOutput vd) where
  lfn fid f = MkFn (fid, decode'lpv f)

-- | Call linear function kind.
class CallLinearFn (fnEff :: LinearEffect) where
  callLfn'l :: forall f x xs b g' r v1 vn vd.
    ( YulO4 x (NP xs) b r
    , LinearEffectVersionDelta fnEff ~ vd
    , v1 + vd ~ vn
    -- constraint f, using b xs
    , UncurryNP'Fst f ~ (x:xs)
    , UncurryNP'Snd f ~ b
    -- constraint b
    , LiftFunction b (YulCat'LVV v1 v1 r ()) (P'V vn r) One ~ P'V vn r b
    -- constraint g'
    , LiftFunction (CurryNP (NP xs) b) (P'V v1 r) (P'V vn r) One ~ g'
    -- CurryingNP instance on "NP xs -> b"
    , CurryingNP xs b (P'V v1 r) (P'V vn r) (YulCat'LVV v1 v1 r ()) One
    )
    => Fn fnEff f
    -> (P'V v1 r x ⊸ g')

instance CallLinearFn (VersionedInputOutput vd) where
  callLfn'l :: forall f x xs b g' r v1 vn.
    ( YulO4 x (NP xs) b r
    , v1 + vd ~ vn
    -- constraint f, using b xs
    , UncurryNP'Fst f ~ (x:xs)
    , UncurryNP'Snd f ~ b
    -- constraint b
    , LiftFunction b (YulCat'LVV v1 v1 r ()) (P'V vn r) One ~ P'V vn r b
    -- constraint g'
    , LiftFunction (CurryNP (NP xs) b) (P'V v1 r) (P'V vn r) One ~ g'
    -- CurryingNP instance on "NP xs -> b"
    , CurryingNP xs b (P'V v1 r) (P'V vn r) (YulCat'LVV v1 v1 r ()) One
    )
    => Fn (VersionedInputOutput vd) f
    -> (P'V v1 r x ⊸ g')
  callLfn'l (MkFn t) x =
    dup2'l x &
    \(x', x'') -> curryingNP @_ @_ @(P'V v1 r) @(P'V vn r) @(YulCat'LVV v1 v1 r ()) @One $
    \(MkYulCat'LVV fxs) -> encode'lvv (YulJmpU t)
                           (cons'l x' (fxs (discard x'')))

instance CallLinearFn (PureInputVersionedOutput vd) where
  callLfn'l :: forall f x xs b g' r v1 vn.
    ( YulO4 x (NP xs) b r
    , v1 + vd ~ vn
    -- constraint f, using b xs
    , UncurryNP'Fst f ~ (x:xs)
    , UncurryNP'Snd f ~ b
    -- constraint b
    , LiftFunction b (YulCat'LVV v1 v1 r ()) (P'V vn r) One ~ P'V vn r b
    -- constraint g'
    , LiftFunction (CurryNP (NP xs) b) (P'V v1 r) (P'V vn r) One ~ g'
    -- CurryingNP instance on "NP xs -> b"
    , CurryingNP xs b (P'V v1 r) (P'V vn r) (YulCat'LVV v1 v1 r ()) One
    )
    => Fn (PureInputVersionedOutput vd) f
    -> (P'V v1 r x ⊸ g')
  callLfn'l f x = callLfn'l @(VersionedInputOutput vd) @f
                  (UnsafeLinear.coerce f) x

-- | Call pure function with pure yul port.
callFn'l :: forall f x xs b g' r eff.
  ( YulO4 x (NP xs) b r
  -- constraint f, using b xs
  , UncurryNP'Fst f ~ (x:xs)
  , UncurryNP'Snd f ~ b
  -- constraint b
  , LiftFunction b (YulCat'LPP r ()) (P'P r) One ~ P'P r b
  -- constraint g'
  , LiftFunction (CurryNP (NP xs) b) (P'P r) (P'P r) One ~ g'
  -- CurryingNP instance on "NP xs -> b"
  , CurryingNP xs b (P'P r) (P'P r) (YulCat'LPP r ()) One
  )
  => Fn (eff :: PureEffectKind) f
  -> (P'P r x ⊸ g')
callFn'l (MkFn t) x =
  dup2'l x &
  \(x', x'') -> curryingNP @_ @_ @(P'P r) @(P'P r) @(YulCat'LPP r ()) @One $
  \(MkYulCat'LPP fxs) -> encode'lpp (YulJmpU t)
                         (cons'l x' (fxs (discard x'')))
