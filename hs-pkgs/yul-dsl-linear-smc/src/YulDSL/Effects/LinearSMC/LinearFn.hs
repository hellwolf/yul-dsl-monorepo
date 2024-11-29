{-# LANGUAGE AllowAmbiguousTypes #-}
module YulDSL.Effects.LinearSMC.LinearFn
  ( LinearEffect (PureInputVersionedOutput, VersionedInputOutput)
  , decode'lvv, decode'lpv, encode'lvv
  , YulCat'LVV (..), unYulCat'LVV, YulCat'LPV (..), unYulCat'LPV, YulCat'LPP (..), unYulCat'LPP
  , uncurry'l, fn'l
  , uncurry'pl, fn'pl
  , call'l
  ) where
-- base
import           GHC.TypeLits
-- linear-base
import           Control.Category.Linear
import           Prelude.Linear
import qualified Unsafe.Linear                       as UnsafeLinear

-- yul-dsl
import           YulDSL.Core
--
import           YulDSL.Effects.LinearSMC.LinearPort


-- | Various types of linear effects for the yul category.
--
-- Note that the pure input pure output linear effect is included. For that, use the pure effect from the 'YulDSL.core'
-- directly.
data LinearEffect = PureInputVersionedOutput Nat -- ^ Pure input ports, versioned output ports
                  | VersionedInputOutput Nat     -- ^ Versioned input and output ports

type instance NonPureEffect (PureInputVersionedOutput vd) = True
type instance NonPureEffect (VersionedInputOutput vd) = True

decode'lvv :: forall a b vd oe. ( YulO2 a b, VersionedInputOutput vd ~ oe)
           => (forall r. YulObj r => P'V 0 r a ⊸ P'V vd r b)
           -> YulCat oe a b
decode'lvv f = decode (h f) -- an intermediate function to fight the multiplicity hell
  where h :: (forall r. YulObj r => P'V 0 r a ⊸ P'V vn r b)
          ⊸ (forall r. YulObj r => P (YulCat oe) r a ⊸ P (YulCat oe) r b)
        h = UnsafeLinear.coerce {- using Unsafe coerce to convert effect after type-checking -}

decode'lpv :: forall a b vd oe. ( YulO2 a b, PureInputVersionedOutput vd ~ oe )
           => (forall r. YulObj r => P'P r a ⊸ P'V vd r b)
           -> YulCat oe a b
decode'lpv f = decode (h f) -- an intermediate function to fight the multiplicity hell
  where h :: (forall r. YulObj r => P'P r a ⊸ P'V vd r b)
          ⊸ (forall r. YulObj r => P (YulCat oe) r a ⊸ P (YulCat oe) r b)
        h = UnsafeLinear.coerce {- using Unsafe coerce to convert effect after type-checking -}

encode'lvv :: forall a b r vd v1. YulO3 a b r
           => YulCat (VersionedInputOutput vd) a b
           -> (P'V v1 r a ⊸ P'V (v1 + vd) r b)
encode'lvv cat x = -- ghc can infer it; annotating for readability and double checking expected types
  let cat' = UnsafeLinear.coerce @_ @(YulCat (VersionedPort v1) a b) cat
  in UnsafeLinear.coerce @(P'V v1 r b) @(P'V (v1 + vd) r b)
     (encode cat' x)

--
-- Operations for VersionedInputOutput
--

-- | Yul category port diagram as a data constructor, otherwise type synonym cannot be partial for @YulCat'LVV r a@.
data YulCat'LVV v1 vn r a b where
  -- ^ Linear input and output ports
  MkYulCat'LVV :: forall a b r v1 vn. YulO3 a b r => (P'V v1 r a ⊸ P'V vn r b) ⊸ YulCat'LVV v1 vn r a b
-- | Unwrap YulCat'LVV linearly.
unYulCat'LVV :: forall a b r v1 vn. YulO3 a b r => YulCat'LVV v1 vn r a b ⊸ (P'V v1 r a ⊸ P'V vn r b)
unYulCat'LVV (MkYulCat'LVV c) = c

-- | Linear yul category morphism with pure inputs to linear outputs.
data YulCat'LPV vn r a b where
  -- ^ Pure input ports to linear output ports
  MkYulCat'LPV :: forall a b r vn. YulO3 a b r => (P'P r a ⊸ P'V vn r b) ⊸ YulCat'LPV vn r a b
-- | Unwrap YulCat'LVV linearly.
unYulCat'LPV :: forall a b r vd. YulO3 a b r => YulCat'LPV vd r a b ⊸ (P'P r a ⊸ P'V vd r b)
unYulCat'LPV (MkYulCat'LPV c) = c

data YulCat'LPP r a b where
  -- ^ Pure input ports to pure output ports
  MkYulCat'LPP :: forall a b r. YulO3 a b r => (P'P r a ⊸ P'P r b) ⊸ YulCat'LPP r a b
-- | Unwrap YulCat'LPP linearly.
unYulCat'LPP :: forall a b r. YulO3 a b r => YulCat'LPP r a b ⊸ (P'P r a ⊸ P'P r b)
unYulCat'LPP (MkYulCat'LPP c) = c

instance forall x v1 vn r a.
         ( YulO3 x r a
         , LiftFunction x (P'V v1 r) (P'V vn r) One ~ P'V vn r x
         ) => UncurryingNP (x) '[] x (P'V v1 r) (P'V vn r) (YulCat'LVV v1 v1 r a) (YulCat'LVV v1 vn r a) One where
  uncurryingNP x (MkYulCat'LVV h) = MkYulCat'LVV
    (\a -> h a &                 -- :: P'V v1 (NP '[])
           coerce'l @_ @() &     -- :: P'V v1 ()
           UnsafeLinear.coerce & -- :: P'V vn ()
           \u -> ignore u x)     -- :: P'V vn x

instance forall x xs b g v1 vn r a.
         ( YulO5 x (NP xs) b r a
         , UncurryingNP g xs b (P'V v1 r) (P'V vn r) (YulCat'LVV v1 v1 r a) (YulCat'LVV v1 vn r a) One
         ) => UncurryingNP (x -> g) (x:xs) b (P'V v1 r) (P'V vn r) (YulCat'LVV v1 v1 r a) (YulCat'LVV v1 vn r a) One where
  uncurryingNP f (MkYulCat'LVV h) = MkYulCat'LVV
    (\xxs ->
        dup2'l xxs
      & \(xxs1, xxs2) -> split (coerce'l @(NP (x:xs)) @(x, NP xs) (h xxs1))
      & \(x, xs) -> unYulCat'LVV
                    (uncurryingNP
                      @g @xs @b
                      @(P'V v1 r) @(P'V vn r) @(YulCat'LVV v1 v1 r a) @(YulCat'LVV v1 vn r a) @One
                      (f x)
                      (MkYulCat'LVV (\a -> ignore (discard a) xs))
                    )
                    xxs2
    )

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

instance forall x v1 vn r a.
         ( YulO3 x r a
         , LiftFunction (CurryNP (NP '[]) x) (P'V v1 r) (P'V vn r) One ~ P'V vn r x
         ) => CurryingNP '[] x (P'V v1 r) (P'V vn r) (YulCat'LVV v1 v1 r a) One where
  curryingNP cb = cb (MkYulCat'LVV (\a -> coerce'l (discard a)))

instance forall x xs b r a v1 vn.
         ( YulO5 x (NP xs) b r a
         , CurryingNP xs b (P'V v1 r) (P'V vn r) (YulCat'LVV v1 v1 r a) One
         ) => CurryingNP (x:xs) b (P'V v1 r) (P'V vn r) (YulCat'LVV v1 v1 r a) One where
  curryingNP cb x = curryingNP @xs @b @(P'V v1 r) @(P'V vn r) @(YulCat'LVV v1 v1 r a) @One
                    (\(MkYulCat'LVV fxs) -> cb (MkYulCat'LVV (\a -> (cons'l x (fxs a)))))

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

instance forall x vd r a.
         ( YulO3 x r a
         , LiftFunction x (P'P r) (P'V vd r) One ~ P'V vd r x
         ) => UncurryingNP (x) '[] x (P'P r) (P'V vd r) (YulCat'LPP r a) (YulCat'LPV vd r a) One where
  uncurryingNP x (MkYulCat'LPP h) = MkYulCat'LPV (\a -> h a &                -- :: P'V v1 (NP '[])
                                                       coerce'l @_ @() &     -- :: P'V v1 ()
                                                       UnsafeLinear.coerce & -- :: P'V vn ()
                                                       \u -> ignore u x)     -- :: P'V vn x

instance forall x xs b g vd r a.
         ( YulO5 x (NP xs) b r a
         , UncurryingNP g xs b (P'P r) (P'V vd r) (YulCat'LPP r a) (YulCat'LPV vd r a) One
         ) => UncurryingNP (x -> g) (x:xs) b (P'P r) (P'V vd r) (YulCat'LPP r a) (YulCat'LPV vd r a) One where
  uncurryingNP f (MkYulCat'LPP h) = MkYulCat'LPV
    (\xxs ->
        dup2'l xxs
      & \(xxs1, xxs2) -> split (coerce'l @(NP (x:xs)) @(x, NP xs) (h xxs1))
      & \(x, xs) -> unYulCat'LPV
                    (uncurryingNP
                      @g @xs @b
                      @(P'P r) @(P'V vd r) @(YulCat'LPP r a) @(YulCat'LPV vd r a) @One
                      (f x)
                      (MkYulCat'LPP (\a -> ignore (discard a) xs))
                    )
                    xxs2
    )

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

instance forall x v1 vn r a.
         ( YulO3 x r a
         , LiftFunction (CurryNP (NP '[]) x) (P'P r) (P'V vn r) One ~ P'V vn r x
         ) => CurryingNP '[] x (P'P r) (P'V vn r) (YulCat'LVV v1 v1 r a) One where
  curryingNP cb = cb (MkYulCat'LVV (\a -> coerce'l (discard a)))

instance forall x xs b r a v1 vn.
         ( YulO5 x (NP xs) b r a
         , CurryingNP xs b (P'P r) (P'V vn r) (YulCat'LVV v1 v1 r a) One
         ) => CurryingNP (x:xs) b (P'P r) (P'V vn r) (YulCat'LVV v1 v1 r a) One where
  curryingNP cb x = curryingNP @xs @b @(P'P r) @(P'V vn r) @(YulCat'LVV v1 v1 r a) @One
                    (\(MkYulCat'LVV fxs) -> cb (MkYulCat'LVV (\a -> (cons'l (lift'pl x) (fxs a)))))
