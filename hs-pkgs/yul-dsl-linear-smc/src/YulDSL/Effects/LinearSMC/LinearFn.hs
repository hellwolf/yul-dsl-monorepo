{-# LANGUAGE AllowAmbiguousTypes #-}
module YulDSL.Effects.LinearSMC.LinearFn
  ( LinearEffect (PureInputVersionedOutput, VersionedInputOutput)
  , uncurry'l, fn'l
  , uncurry'pl, fn'pl
  , call'l
  ) where
-- base
import           GHC.TypeLits
-- linear-base
import           Control.Category.Linear
import           Prelude.Linear

-- yul-dsl
import           YulDSL.Core
--
import           YulDSL.Effects.LinearSMC.LinearPort
import           YulDSL.Effects.LinearSMC.LinearYulCat


--
-- Operations for VersionedInputOutput
--

uncurry'l :: forall f xs b r vd m1 m1b m2 m2b.
             ( YulO3 (NP xs) b r
             , xs ~ UncurryNP'Fst f
             , b  ~ UncurryNP'Snd f
             , P'V  0 r ~ m1
             , P'V vd r ~ m1b
             , YulCat'LVV 0  0 r (NP xs) ~ m2
             , YulCat'LVV 0 vd r (NP xs) ~ m2b
             , UncurryingNP f xs b m1 m1b m2 m2b One
             , LiftFunction (NP xs -> b) m1 m1b One ~ (P'V 0 r (NP xs) ⊸ P'V vd r b)
             , LiftFunction b m2 m2b One ~ m2b b
             )
          => LiftFunction           f  m1 m1b One
          -> LiftFunction (NP xs -> b) m1 m1b One
uncurry'l f = unYulCat'LVV (uncurryingNP @f @xs @b @m1 @m1b @m2 @m2b @One f (MkYulCat'LVV id))

-- | Define a `YulCat` morphism from a linear port function.
fn'l :: forall f xs b vd.
        ( YulO2 (NP xs) b
        , CurryNP (NP xs) b ~ f
        , UncurryNP'Fst f ~ xs
        , UncurryNP'Snd f ~ b
        )
     => String
     -> (forall r. YulObj r => P'V 0 r (NP xs) ⊸ P'V vd r b)
     -> Fn (VersionedInputOutput vd) (CurryNP (NP xs) b)
fn'l fid f = MkFn (MkFnCat fid (decode'lvv f))

call'l :: forall f x xs b g' r v1 vd vn.
          ( YulO4 x (NP xs) b r
          , UncurryNP'Fst f ~ (x:xs)
          , UncurryNP'Snd f ~ b
          , v1 + vd ~ vn
          , LiftFunction (CurryNP (NP xs) b) (P'V v1 r) (P'V vn r) One ~ g'
          , CurryingNP xs b (P'V v1 r) (P'V vn r) (YulCat'LVV v1 v1 r ()) One
          , LiftFunction b (YulCat'LVV v1 v1 r ()) (P'V vn r) One ~ P'V vn r b
          )
       => Fn (VersionedInputOutput vd) f
       -> (P'V v1 r x ⊸ g')
call'l (MkFn f) x =
   dup2'l x & \(x', x'') ->
       curryingNP
       @xs @b @(P'V v1 r) @(P'V vn r) @(YulCat'LVV v1 v1 r ()) @One
       (\(MkYulCat'LVV fxs) -> encode'lvv (YulJump (fnId f) (fnCat f)) (cons'l x' (fxs (discard x''))))

--
-- Operations for PureInputVersionedOutput
--

-- | Define a `YulCat` morphism from a linear port function.
fn'pl :: forall f xs b vd.
        ( YulO2 (NP xs) b
        , CurryNP (NP xs) b ~ f
        , UncurryNP'Fst f ~ xs
        , UncurryNP'Snd f ~ b
        )
     => String
     -> (forall r. YulObj r => P'P r (NP xs) ⊸ P'V vd r b)
     -> Fn (PureInputVersionedOutput vd) (CurryNP (NP xs) b)
fn'pl fid f = MkFn (MkFnCat fid (decode'lpv f))

uncurry'pl :: forall f xs b r vd m1 m1b m2 m2b.
              ( YulO3 (NP xs) b r
              , xs ~ UncurryNP'Fst f
              , b  ~ UncurryNP'Snd f
              , P'P    r ~ m1
              , P'V vd r ~ m1b
              , YulCat'LPP r (NP xs) ~ m2
              , YulCat'LPV vd r (NP xs) ~ m2b
              , UncurryingNP f xs b m1 m1b m2 m2b One
              , LiftFunction b m2 m2b One ~ m2b b
              )
           => LiftFunction f m1 m1b One
           -> (P'P r (NP xs) ⊸ P'V vd r b)
uncurry'pl f = unYulCat'LPV (uncurryingNP @f @xs @b @m1 @m1b @m2 @m2b @One f (MkYulCat'LPP id))
