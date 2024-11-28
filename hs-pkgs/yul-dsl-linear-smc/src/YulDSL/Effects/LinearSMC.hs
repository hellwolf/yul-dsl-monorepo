{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
module YulDSL.Effects.LinearSMC where

-- base
import           GHC.TypeError                                 (Assert, TypeError)
import           GHC.TypeLits                                  (ErrorMessage (Text), type (+), type (<=))
-- linear-base
import           Prelude.Linear                                hiding (Eq (..), Ord (..))
import qualified Unsafe.Linear                                 as UnsafeLinear
-- linear-smc
import           Control.Category.Constrained                  ()
import           Control.Category.Linear
    ( P
    , copy
    , decode
    , discard
    , encode
    , ignore
    , merge
    , mkUnit
    , split
    )
-- yul-dsl
import           YulDSL.Core
-- orphansed instances for categories in linear-smc
import           Control.Category.Constrained.YulDSL.LinearSMC ()
import           Data.MPOrd.YulDSL.LinearSMC

--
-- Linear effects and linear port
--

-- | Various types of linear effects for the yul category.
--
-- Note that the pure input pure output linear effect is included. For that, use the pure effect from the 'YulDSL.core'
-- directly.
data LinearEffect = PureInputLinearOutput Nat -- ^ Pure input ports, linear output ports
                  | LinearInputOutput Nat     -- ^ Linear input and output ports

type instance NonPureEffect (PureInputLinearOutput vd) = True
type instance NonPureEffect (LinearInputOutput vd) = True

-- | Various types of linear effects for the linearly port API.
data PortEffect = PurePort         -- ^ Pure port that does not need to be versioned
                | LinearPort Nat   -- ^ Linearly versioned port
                | TerminalPort Nat -- ^ Termination port

type instance NonPureEffect PurePort = False
type instance NonPureEffect (LinearPort v) = True
type family IsNotTerminalPort (a :: PortEffect) :: Bool where
  IsNotTerminalPort (TerminalPort _) = False
  IsNotTerminalPort _ = True
type AssertNotTerminalPort a = Assert (IsNotTerminalPort a) (TypeError (Text "Terminal port reached"))

-- | Linear port of yul categories with linear effect kind.
type P'xL (eff :: PortEffect) = P (YulCat eff)

-- | Linear port of yul category with linearly versioned data.
type P'L v = P'xL (LinearPort v)

-- | Linear port of yul category with pure data.
type P'PL = P'xL PurePort

type P'TL v = P'xL (TerminalPort v)

type CPS'xL ie vn r a b = (P'xL ie r b ⊸ P'TL vn r b) ⊸ P'TL vn r b

-- | Yul category port diagram as a data constructor, otherwise type synonym cannot be partial for @YulCat'L r a@.
data YulCat'L v1 vn r a b where
  -- ^ Linear input and output ports
  MkYulCat'L  :: forall a b r v1 vn. YulO3 a b r => (P'L v1 r a ⊸ P'L vn r b) ⊸ YulCat'L v1 vn r a b

-- | Unwrap YulCat'L linearly.
unYulCat'L :: forall a b r v1 vn. YulO3 a b r => YulCat'L v1 vn r a b ⊸ (P'L v1 r a ⊸ P'L vn r b)
unYulCat'L (MkYulCat'L c) = c

data YulCat'PL vn r a b where
  -- ^ Pure input ports to linear output ports
  MkYulCat'PL :: forall a b r vn. YulO3 a b r => (P'PL r a ⊸ P'L vn r b) ⊸ YulCat'PL vn r a b

-- | Unwrap YulCat'L linearly.
unYulCat'PL :: forall a b r vd. YulO3 a b r => YulCat'PL vd r a b ⊸ (P'PL r a ⊸ P'L vd r b)
unYulCat'PL (MkYulCat'PL c) = c

data YulCat'PPL r a b where
  -- ^ Pure input ports to pure output ports
  MkYulCat'PPL :: forall a b r. YulO3 a b r => (P'PL r a ⊸ P'PL r b) ⊸ YulCat'PPL r a b

-- | Unwrap YulCat'L linearly.
unYulCat'PPL :: forall a b r. YulO3 a b r => YulCat'PPL r a b ⊸ (P'PL r a ⊸ P'PL r b)
unYulCat'PPL (MkYulCat'PPL c) = c

lift'pl :: forall a r v. YulO2 a r => P'PL r a ⊸ P'L v r a
lift'pl = UnsafeLinear.coerce

decode'l :: forall a b vd. YulO2 a b
         => (forall r. YulObj r => P'L 0 r a ⊸ P'L vd r b)
         -> YulCat (LinearInputOutput vd) a b
decode'l f = decode (h f) -- an intermediate function to fight the multiplicity hell
  where h :: (forall r. YulObj r => P'L 0 r a ⊸ P'L vn r b)
          ⊸ (forall r. YulObj r => P (YulCat (LinearInputOutput vn)) r a ⊸ P (YulCat (LinearInputOutput vn)) r b)
        h = UnsafeLinear.coerce {- using Unsafe coerce to convert effect after type-checking -}

decode'pl :: forall a b vd. YulO2 a b
          => (forall r. YulObj r => P'PL r a ⊸ P'L vd r b)
          -> YulCat (PureInputLinearOutput vd) a b
decode'pl f = decode (h f) -- an intermediate function to fight the multiplicity hell
  where h :: (forall r. YulObj r => P'PL r a ⊸ P'L vd r b)
          ⊸ (forall r. YulObj r => P (YulCat (PureInputLinearOutput vn)) r a ⊸ P (YulCat (PureInputLinearOutput vn)) r b)
        h = UnsafeLinear.coerce {- using Unsafe coerce to convert effect after type-checking -}

encode'l :: forall a b r vd v1. YulO3 a b r
         => YulCat (LinearInputOutput vd) a b
         -> (P'L v1 r a ⊸ P'L (v1 + vd) r b)
encode'l cat x = -- ghc can infer it; annotating for readability and double checking expected types
  let cat' = UnsafeLinear.coerce @_ @(YulCat (LinearPort v1) a b) cat
  in UnsafeLinear.coerce @(P'L v1 r b) @(P'L (v1 + vd) r b)
     (encode cat' x)

---
-- Linear port operations
--

emb'l :: forall a d eff r. (AssertNotTerminalPort eff, YulO3 a d r)
        => a -> (P'xL eff r d ⊸ P'xL eff r a)
emb'l a = encode (YulEmbed a) . discard

dup'l :: forall a eff r. (AssertNotTerminalPort eff, YulO2 a r)
      => P'xL eff r a ⊸ (P'xL eff r a, P'xL eff r a)
dup'l = split . copy

use'l :: forall a b eff r. (AssertNotTerminalPort eff, YulO3 a b r)
      => P'xL eff r a ⊸ (P'xL eff r a ⊸ P'xL eff r b) ⊸ (P'xL eff r a, P'xL eff r b)
use'l a f = dup'l a & \(a', a'') -> (a', f a'')

dis'l :: forall a eff r. (AssertNotTerminalPort eff, YulO2 a r)
      => P'xL eff r a ⊸ P'xL eff r ()
dis'l = discard

const'l :: forall a b eff r. (AssertNotTerminalPort eff, YulO3 a b r)
      => P'xL eff r a ⊸ P'xL eff r b ⊸ P'xL eff r a
const'l = flip (ignore . discard)

coerce'l :: forall a b eff r. (AssertNotTerminalPort eff, YulO3 a b r, ABITypeCoercible a b)
         => P'xL eff r a ⊸ P'xL eff r b
coerce'l = encode YulCoerce

cons'l :: forall x xs eff r. (AssertNotTerminalPort eff, YulO3 x (NP xs) r)
       => P'xL eff r x ⊸ P'xL eff r (NP xs) ⊸ P'xL eff r (NP (x:xs))
cons'l x xs = coerce'l (merge (x, xs))

--
-- Operations for LinearInputOutput
--

instance forall x v1 vn r a.
         ( YulO3 x r a
         , LiftFunction x (P'L v1 r) (P'L vn r) One ~ P'L vn r x
         ) => UncurryingNP (x) '[] x (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r a) (YulCat'L v1 vn r a) One where
  uncurryingNP x (MkYulCat'L h) = MkYulCat'L (\a -> h a &                 -- :: P'L v1 (NP '[])
                                                    coerce'l @_ @() &     -- :: P'L v1 ()
                                                    UnsafeLinear.coerce & -- :: P'L vn ()
                                                    \u -> ignore u x)     -- :: P'L vn x

instance forall x xs b g v1 vn r a.
         ( YulO5 x (NP xs) b r a
         , UncurryingNP g xs b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r a) (YulCat'L v1 vn r a) One
         ) => UncurryingNP (x -> g) (x:xs) b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r a) (YulCat'L v1 vn r a) One where
  uncurryingNP f (MkYulCat'L h) = MkYulCat'L
    (\xxs -> dup'l xxs &
             \(xxs1, xxs2) -> split (coerce'l @(NP (x:xs)) @(x, NP xs) (h xxs1)) &
             \(x, xs) -> unYulCat'L
                         (uncurryingNP
                           @g @xs @b
                           @(P'L v1 r) @(P'L vn r) @(YulCat'L v1 v1 r a) @(YulCat'L v1 vn r a) @One
                           (f x)
                           (MkYulCat'L (\a -> ignore (discard a) xs))
                         )
                         xxs2
    )

uncurry'l :: forall f xs b r vd m1 m1b m2 m2b.
             ( YulO3 (NP xs) b r
             , xs ~ UncurryNP'Fst f
             , b  ~ UncurryNP'Snd f
             , P'L  0 r ~ m1
             , P'L vd r ~ m1b
             , YulCat'L 0  0 r (NP xs) ~ m2
             , YulCat'L 0 vd r (NP xs) ~ m2b
             , UncurryingNP f xs b m1 m1b m2 m2b One
             , LiftFunction (NP xs -> b) m1 m1b One ~ (P'L 0 r (NP xs) ⊸ P'L vd r b)
             , LiftFunction b m2 m2b One ~ m2b b
             )
          => LiftFunction           f  m1 m1b One
          -> LiftFunction (NP xs -> b) m1 m1b One
uncurry'l f = unYulCat'L (uncurryingNP @f @xs @b @m1 @m1b @m2 @m2b @One f (MkYulCat'L id))

-- | Define a `YulCat` morphism from a linear port function.
fn'l :: forall f xs b vd.
        ( YulO2 (NP xs) b
        , CurryNP (NP xs) b ~ f
        , UncurryNP'Fst f ~ xs
        , UncurryNP'Snd f ~ b
        )
     => String
     -> (forall r. YulObj r => P'L 0 r (NP xs) ⊸ P'L vd r b)
     -> Fn (LinearInputOutput vd) (CurryNP (NP xs) b)
fn'l fid f = MkFn (MkFnCat fid (decode'l f))

instance forall x v1 vn r a.
         ( YulO3 x r a
         , LiftFunction (CurryNP (NP '[]) x) (P'L v1 r) (P'L vn r) One ~ P'L vn r x
         ) => CurryingNP '[] x (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r a) One where
  curryingNP cb = cb (MkYulCat'L (\a -> coerce'l (discard a)))

instance forall x xs b r a v1 vn.
         ( YulO5 x (NP xs) b r a
         , CurryingNP xs b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r a) One
         ) => CurryingNP (x:xs) b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r a) One where
  curryingNP cb x = curryingNP @xs @b @(P'L v1 r) @(P'L vn r) @(YulCat'L v1 v1 r a) @One
                    (\(MkYulCat'L fxs) -> cb (MkYulCat'L (\a -> (cons'l x (fxs a)))))

call'l :: forall f x xs b g' r v1 vd vn.
          ( YulO4 x (NP xs) b r
          , UncurryNP'Fst f ~ (x:xs)
          , UncurryNP'Snd f ~ b
          , v1 + vd ~ vn
          , LiftFunction (CurryNP (NP xs) b) (P'L v1 r) (P'L vn r) One ~ g'
          , CurryingNP xs b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r ()) One
          , LiftFunction b (YulCat'L v1 v1 r ()) (P'L vn r) One ~ P'L vn r b
          )
       => Fn (LinearInputOutput vd) f
       -> (P'L v1 r x ⊸ g')
call'l (MkFn f) x = dup'l x & \(x', x'') ->
  curryingNP @xs @b @(P'L v1 r) @(P'L vn r) @(YulCat'L v1 v1 r ()) @One
  (\(MkYulCat'L fxs) -> encode'l (YulJump (fnId f) (fnCat f)) (cons'l x' (fxs (discard x''))))

--
-- Operations for PureInputLinearOutput
--

instance forall x vd r a.
         ( YulO3 x r a
         , LiftFunction x (P'PL r) (P'L vd r) One ~ P'L vd r x
         ) => UncurryingNP (x) '[] x (P'PL r) (P'L vd r) (YulCat'PPL r a) (YulCat'PL vd r a) One where
  uncurryingNP x (MkYulCat'PPL h) = MkYulCat'PL (\a -> h a &                 -- :: P'L v1 (NP '[])
                                                       coerce'l @_ @() &     -- :: P'L v1 ()
                                                       UnsafeLinear.coerce & -- :: P'L vn ()
                                                       \u -> ignore u x)     -- :: P'L vn x

instance forall x xs b g vd r a.
         ( YulO5 x (NP xs) b r a
         , UncurryingNP g xs b (P'PL r) (P'L vd r) (YulCat'PPL r a) (YulCat'PL vd r a) One
         ) => UncurryingNP (x -> g) (x:xs) b (P'PL r) (P'L vd r) (YulCat'PPL r a) (YulCat'PL vd r a) One where
  uncurryingNP f (MkYulCat'PPL h) = MkYulCat'PL
    (\xxs -> dup'l xxs &
             \(xxs1, xxs2) -> split (coerce'l @(NP (x:xs)) @(x, NP xs) (h xxs1)) &
             \(x, xs) -> unYulCat'PL
                         (uncurryingNP
                           @g @xs @b
                           @(P'PL r) @(P'L vd r) @(YulCat'PPL r a) @(YulCat'PL vd r a) @One
                           (f x)
                           (MkYulCat'PPL (\a -> ignore (discard a) xs))
                         )
                         xxs2
    )

uncurry'pl :: forall f xs b r vd m1 m1b m2 m2b.
              ( YulO3 (NP xs) b r
              , xs ~ UncurryNP'Fst f
              , b  ~ UncurryNP'Snd f
              , P'PL   r ~ m1
              , P'L vd r ~ m1b
              , YulCat'PPL r (NP xs) ~ m2
              , YulCat'PL vd r (NP xs) ~ m2b
              , UncurryingNP f xs b m1 m1b m2 m2b One
              , LiftFunction b m2 m2b One ~ m2b b
              )
           => LiftFunction f m1 m1b One
           -> (P'PL r (NP xs) ⊸ P'L vd r b)
uncurry'pl f = unYulCat'PL (uncurryingNP @f @xs @b @m1 @m1b @m2 @m2b @One f (MkYulCat'PPL id))

-- | Define a `YulCat` morphism from a linear port function.
fn'pl :: forall f xs b vd.
        ( YulO2 (NP xs) b
        , CurryNP (NP xs) b ~ f
        , UncurryNP'Fst f ~ xs
        , UncurryNP'Snd f ~ b
        )
     => String
     -> (forall r. YulObj r => P'PL r (NP xs) ⊸ P'L vd r b)
     -> Fn (PureInputLinearOutput vd) (CurryNP (NP xs) b)
fn'pl fid f = MkFn (MkFnCat fid (decode'pl f))

instance forall x v1 vn r a.
         ( YulO3 x r a
         , LiftFunction (CurryNP (NP '[]) x) (P'PL r) (P'L vn r) One ~ P'L vn r x
         ) => CurryingNP '[] x (P'PL r) (P'L vn r) (YulCat'L v1 v1 r a) One where
  curryingNP cb = cb (MkYulCat'L (\a -> coerce'l (discard a)))

instance forall x xs b r a v1 vn.
         ( YulO5 x (NP xs) b r a
         , CurryingNP xs b (P'PL r) (P'L vn r) (YulCat'L v1 v1 r a) One
         ) => CurryingNP (x:xs) b (P'PL r) (P'L vn r) (YulCat'L v1 v1 r a) One where
  curryingNP cb x = curryingNP @xs @b @(P'PL r) @(P'L vn r) @(YulCat'L v1 v1 r a) @One
                    (\(MkYulCat'L fxs) -> cb (MkYulCat'L (\a -> (cons'l (lift'pl x) (fxs a)))))


--
-- Storage utilities
--

sget :: forall a r v. (YulO2 a r, ABIWordValue a)
     => P'L v r ADDR ⊸ P'L v r a
sget = encode YulSGet

sput :: forall a b r v1 vd. (YulO3 a b r, ABIWordValue a, v1 + 1 <= vd)
     => P'L v1 r ADDR ⊸ P'L v1 r a ⊸ (P'L (v1 + 1) r a ⊸ P'L vd r b) ⊸ P'L vd r b
sput to x f = dup'l x
  & \(x', x'') -> encode YulSPut (merge (to, x')) &
  -- increase port version by one
  \u -> UnsafeLinear.coerce @(P'L v1 r a) @(P'L (v1 + 1) r a) (ignore u x'')
  & f

sputAt :: forall a b r v1 vd. (YulO3 a b r, ABIWordValue a, v1 + 1 <= vd)
       => ADDR -> P'L v1 r a ⊸ (P'L (v1 + 1) r a ⊸ P'L vd r b) ⊸ P'L vd r b
sputAt to x f = mkUnit x & \(x', u) -> emb'l to u & \a -> sput a x' f

--
-- Prelude type class instances
--

-- | 'MPEq' instance for linear yul ports.
instance (YulObj r, YulNum a) => MPEq (P'xL eff r a) (P'xL eff r BOOL) where
  a == b = encode (YulNumCmp (false, true , false)) (merge (a, b))
  a /= b = encode (YulNumCmp (true , false, true )) (merge (a, b))

-- | 'MPOrd' instance for linear yul ports.
instance (YulObj r, YulNum a) => MPOrd (P'xL eff r a) (P'xL eff r BOOL) where
  a  < b = encode (YulNumCmp (true , false, false)) (merge (a, b))
  a <= b = encode (YulNumCmp (true , true , false)) (merge (a, b))
  a  > b = encode (YulNumCmp (false, false, true )) (merge (a, b))
  a >= b = encode (YulNumCmp (false, true , true )) (merge (a, b))

instance YulO2 a r => IfThenElse (P'xL eff r BOOL) (P'xL eff r a) where
  ifThenElse c a b = encode YulITE (merge(c, merge(a, b)))
