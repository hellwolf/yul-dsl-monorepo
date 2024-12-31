{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
module YulDSL.Effects.LinearSMC.LinearFn
  ( -- * Linear Yul Function Builders
    -- $LinearFn
    fn'l
    -- * Call Yul Functions Via YulMonad
    -- $CallFn
  , call'l
  -- , call
  ) where
-- base
import GHC.TypeLits                          (type (+))
-- linear-base
import Control.Category.Linear               (discard)
import Prelude.Linear
-- yul-dsl
import YulDSL.Core
-- linearly-versioned-monad
import Control.LinearlyVersionedMonad        qualified as LVM
--
import YulDSL.Effects.LinearSMC.LinearYulCat
import YulDSL.Effects.LinearSMC.YulMonad
import YulDSL.Effects.LinearSMC.YulPort


------------------------------------------------------------------------------------------------------------------------
-- $LinearFn
------------------------------------------------------------------------------------------------------------------------

-- | Create linear functions.
class LinearFnLike (iEff :: PortEffect) (oEff :: PortEffect) (fnEff :: LinearEffect) | iEff oEff -> fnEff where
  -- | Define a `YulCat` morphism from a yul port diagram.
  fn'l :: forall f xs b.
    ( YulO2 (NP xs) b
    -- constraint f, using b xs
    , CurryNP (NP xs) b ~ f
    , UncurryNP'Fst f ~ xs
    , UncurryNP'Snd f ~ b
    )
    => String
    -> (forall r. YulO1 r => P'x iEff r (NP xs) ⊸ P'x oEff r b)
    -> Fn fnEff (CurryNP (NP xs) b)

instance forall vd. LinearFnLike (VersionedPort 0) (VersionedPort vd) (VersionedInputOutput vd) where
  fn'l fid f = MkFn (fid, decode'lvv f)

instance forall vd. LinearFnLike PurePort (VersionedPort vd) (PureInputVersionedOutput vd) where
  fn'l fid f = MkFn (fid, decode'lpv f)

------------------------------------------------------------------------------------------------------------------------
-- $CallFn
------------------------------------------------------------------------------------------------------------------------

call'l :: forall f x xs b g' r v1 vn vd.
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
call'l (MkFn t) x =
  dup2'l x &
  \(x', x'') -> curryingNP @xs @b @(P'V v1 r) @(P'V vn r) @(YulCat'LVV v1 v1 r ()) @One $
  \(MkYulCat'LVV fxs) -> encode'lvv (YulJmpU t)
                         (cons'l x' (fxs (discard x'')))

-- call :: forall f x xs b g' r v1 vn vd f' b'.
--   ( YulO4 x (NP xs) b r
--   , v1 + vd ~ vn
--   -- constraint f, using b xs
--   , UncurryNP'Fst f ~ (x:xs)
--   , UncurryNP'Snd f ~ b
--   -- constraint b'
--   , P'V vd r b ~ b'
--   , LiftFunction b' (YulCat'LVM v1 v1 r ()) (YulMonad v1 vn r) One ~ YulMonad v1 vn r b'
--   -- constraint f'
--   , UncurryNP'Fst f'   ~ xs
--   , UncurryNP'Snd f'   ~ b'
--   -- constraint g'
--   , LiftFunction (CurryNP (NP xs) b) (YulMonad v1 vn r) (YulMonad v1 vn r) One ~ g'
--   -- CurryingNP instance on "NP xs -> b"
--   , CurryingNP xs b' (P'V v1 r) (YulMonad v1 vn r) (YulCat'LVM v1 v1 r ()) One
--   )
--   => Fn (VersionedInputOutput vd) f
--   -> (P'V v1 r x ⊸ g')
-- call (MkFn t) x =
--   dup2'l x &
--   \(x', x'') ->
--     curryingNP @xs @b' @(P'V v1 r) @(YulMonad v1 vn r) @(YulCat'LVM v1 v1 r ()) @One $
--     \(MkYulCat'LVM fxs) -> LVM.do
--     xs <- fxs (discard x'')
--     LVM.pure $ encode'lvv (YulJmpU t) (cons'l x' xs)
