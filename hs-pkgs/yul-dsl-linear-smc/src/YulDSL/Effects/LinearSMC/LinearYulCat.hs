module YulDSL.Effects.LinearSMC.LinearYulCat
  ( LinearEffect (PureInputVersionedOutput, VersionedInputOutput)
    -- $yul_port_diagrams
  , YulCat'LVV (MkYulCat'LVV), YulCat'LPV (MkYulCat'LPV), YulCat'LPP (MkYulCat'LPP)
  , decode'lvv, decode'lpv, encode'lvv
  ) where
-- base
import GHC.TypeLits
-- linear-base
import Control.Category.Linear          (P, decode, discard, encode, ignore, split)
import Prelude.Linear
import Unsafe.Linear                    qualified as UnsafeLinear
-- yul-dsl
import YulDSL.Core
--
import YulDSL.Effects.LinearSMC.YulPort


-- | Various types of linear effects for the yul category.
--
-- Note that the pure input pure output linear effect is included. For that, use the pure effect from the 'YulDSL.core'
-- directly.
data LinearEffect = PureInputVersionedOutput Nat -- ^ Pure input ports, versioned output ports
                  | VersionedInputOutput Nat     -- ^ Versioned input and output ports

type instance NonPureEffect (PureInputVersionedOutput vd) = True
type instance NonPureEffect (VersionedInputOutput vd) = True

-- $yul_port_diagrams
-- = Yul Port Diagrams
--
-- A yul port diagram is a morphism in the yul category represented by one input yul port and one output yul port.
--
-- There are three variants with different purity for input and output ports as their names suggest, where V means
-- versioned and P means pure.
--
-- Additionally, they are data types instead of type synonyms because of GHC's lack of type-level lambda support. As a
-- result, each of them also comes with an unYulCat function to unwrap them linearly.

-- | Yul port diagram for versioned input and outputs.
newtype YulCat'LVV v1 vn r a b = MkYulCat'LVV (P'V v1 r a ⊸ P'V vn r b)

-- | Yul port diagram for pure input and versioned outputs.
newtype YulCat'LPV vn r a b = MkYulCat'LPV (P'P r a ⊸ P'V vn r b)

-- | Yul port diagram for pure input and pure outputs.
newtype YulCat'LPP r a b = MkYulCat'LPP (P'P r a ⊸ P'P r b)

decode'lvv :: forall a b vd. YulO2 a b
           => (forall r. YulO1 r => P'V 0 r a ⊸ P'V vd r b)
           -> YulCat (VersionedInputOutput vd) a b
decode'lvv f = decode (h f) -- an intermediate function to fight the multiplicity hell
  where h :: (forall r. YulO1 r => P'V 0 r a ⊸ P'V vn r b)
          ⊸ (forall r. YulO1 r => P (YulCat oe) r a ⊸ P (YulCat oe) r b)
        h = UnsafeLinear.coerce {- using Unsafe coerce to convert effect after type-checking -}

decode'lpv :: forall a b vd. YulO2 a b
           => (forall r. YulO1 r => P'P r a ⊸ P'V vd r b)
           -> YulCat (PureInputVersionedOutput vd) a b
decode'lpv f = decode (h f) -- an intermediate function to fight the multiplicity hell
  where h :: (forall r. YulO1 r => P'P r a ⊸ P'V vd r b)
          ⊸ (forall r. YulO1 r => P (YulCat oe) r a ⊸ P (YulCat oe) r b)
        h = UnsafeLinear.coerce {- using Unsafe coerce to convert effect after type-checking -}

encode'lvv :: forall a b r vd v1. YulO3 a b r
           => YulCat (VersionedInputOutput vd) a b
           -> (P'V v1 r a ⊸ P'V (v1 + vd) r b)
encode'lvv cat x = -- ghc can infer it; annotating for readability and double checking expected types
  let cat' = UnsafeLinear.coerce cat :: YulCat (VersionedPort v1) a b
  in UnsafeLinear.coerce @(P'V v1 r b) @(P'V (v1 + vd) r b)
     (encode cat' x)

--
-- (P'V ⊸ P'V ⊸ ... P'V) <-> YulCat'LVV ⊸ YulCat'LVV
--

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
      & \(x, xs) -> let !(MkYulCat'LVV g) =
                          (uncurryingNP
                            @g @xs @b
                            @(P'V v1 r) @(P'V vn r) @(YulCat'LVV v1 v1 r a) @(YulCat'LVV v1 vn r a) @One
                            (f x)
                            (MkYulCat'LVV (\a -> ignore (discard a) xs))
                          )
                    in g xxs2
    )

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

--
-- (P'P ⊸ P'P ⊸ ... P'V) <=> YulCat'LPP ⊸ YulCat'LPV
--

instance forall x vd r a.
         ( YulO3 x r a
         , LiftFunction x (P'P r) (P'V vd r) One ~ P'V vd r x
         ) => UncurryingNP (x) '[] x (P'P r) (P'V vd r) (YulCat'LPP r a) (YulCat'LPV vd r a) One where
  uncurryingNP x (MkYulCat'LPP h) = MkYulCat'LPV (\a -> h a &                -- :: P'P (NP '[])
                                                       coerce'l @_ @() &     -- :: P'P ()
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
      & \(x, xs) -> let !(MkYulCat'LPV g) =
                          (uncurryingNP
                           @g @xs @b
                           @(P'P r) @(P'V vd r) @(YulCat'LPP r a) @(YulCat'LPV vd r a) @One
                           (f x)
                           (MkYulCat'LPP (\a -> ignore (discard a) xs))
                          )
                    in g xxs2
    )

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
                    (\(MkYulCat'LVV fxs) -> cb (MkYulCat'LVV (\a -> (cons'l (UnsafeLinear.coerce x) (fxs a)))))
