{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
module YulDSL.Effects.LinearSMC.LinearFn
  ( LinearEffect (PureInputVersionedOutput, VersionedInputOutput)
  , fn'l, uncurry'lv, uncurry'lp, yulmonad'lv, yulmonad'lp
  , call'l
  , match'l
  ) where
-- base
import           GHC.TypeLits                          (type (+))
import qualified Prelude                               as BasePrelude
-- linear-base
import           Control.Category.Linear               (discard, ignore, mkUnit, split)
import           Prelude.Linear
import qualified Unsafe.Linear                         as UnsafeLinear
-- yul-dsl
import           YulDSL.Core
--
import qualified Control.LinearlyVersionedMonad        as LVM
--
import           YulDSL.Effects.LinearSMC.LinearYulCat
import           YulDSL.Effects.LinearSMC.YulMonad
import           YulDSL.Effects.LinearSMC.YulPort


-- $fnl_functions
-- = Create Linear Functions With @fn'l@

class LinearFnKind (ie :: PortEffect) (oe :: PortEffect) (fe :: LinearEffect) | ie oe -> fe where
  -- | Define a `YulCat` morphism from a yul port diagram.
  fn'l :: forall f xs b.
          ( YulO2 (NP xs) b
          , CurryNP (NP xs) b ~ f
          , UncurryNP'Fst f ~ xs
          , UncurryNP'Snd f ~ b
          )
       => String
       -> (forall r. YulO1 r => P'x ie r (NP xs) ⊸ P'x oe r b)
       -> Fn fe (CurryNP (NP xs) b)

instance forall vd. LinearFnKind (VersionedPort 0) (VersionedPort vd) (VersionedInputOutput vd) where
  fn'l fid f = MkFn (MkFnCat fid (decode'lvv f))

instance forall vd. LinearFnKind PurePort (VersionedPort vd) (PureInputVersionedOutput vd) where
  fn'l fid f = MkFn (MkFnCat fid (decode'lpv f))

-- $uncurry_lv
-- = Uncurrying Functions With Linear Port Inputs

uncurry'lv :: forall f xs b r vd m1 m1b m2 m2b.
              ( YulO3 (NP xs) b r
              , xs ~ UncurryNP'Fst f
              , b  ~ UncurryNP'Snd f
              , P'V  0 r ~ m1
              , P'V vd r ~ m1b
              , YulCat'LVV 0  0 r (NP xs) ~ m2
              , YulCat'LVV 0 vd r (NP xs) ~ m2b
              , UncurryingNP f xs b m1 m1b m2 m2b One
              , LiftFunction b m2 m2b One ~ m2b b
              )
           => LiftFunction f m1 m1b One       -- ^ @LiftFunction           f  m1 m1b One@
           -> (P'V 0 r (NP xs) ⊸ P'V vd r b) -- ^ @LiftFunction (NP xs -> b) m1 m1b One@
uncurry'lv f =  let !(MkYulCat'LVV f') = uncurryingNP @f @xs @b @m1 @m1b @m2 @m2b @One f (MkYulCat'LVV id)
                in f'

uncurry'lp :: forall f xs b r vd m1 m1b m2 m2b.
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
uncurry'lp f = let !(MkYulCat'LPV f') = (uncurryingNP @f @xs @b @m1 @m1b @m2 @m2b @One f (MkYulCat'LPP id))
               in f'

--
-- yulmonad'lv
--

-- | Monadic yul port diagrams for versioned input and yul monad output.
newtype YulCat'LVM v1 vn r a b = MkYulCat'LVM (P'V v1 r a ⊸ YulMonad v1 vn r b)

instance forall x v1 vn r a.
         ( YulO3 x r a
         , LiftFunction x (P'V v1 r) (P'V vn r) One ~ P'V vn r x
         ) => UncurryingNP (P'V vn r x) '[] (P'V vn r x)
         (P'V v1 r) (YulMonad v1 vn r)
         (YulCat'LVV v1 v1 r a) (YulCat'LVM v1 vn r a) One where
  uncurryingNP x (MkYulCat'LVV h) = MkYulCat'LVM
    \a -> toss (h a) LVM.>> x

instance forall x xs b g v1 vn r a.
         ( YulO5 x (NP xs) b r a
         , UncurryingNP g xs (P'V vn r b) (P'V v1 r) (YulMonad v1 vn r) (YulCat'LVV v1 v1 r a) (YulCat'LVM v1 vn r a) One
         ) => UncurryingNP (x -> g) (x:xs) (P'V vn r b)
         (P'V v1 r) (YulMonad v1 vn r)
         (YulCat'LVV v1 v1 r a) (YulCat'LVM v1 vn r a) One where
  uncurryingNP f (MkYulCat'LVV h) = MkYulCat'LVM
    (\xxs ->
        dup2'l xxs
      & \(xxs1, xxs2) -> split (coerce'l @(NP (x:xs)) @(x, NP xs) (h xxs1))
      & \(x, xs) -> let !(MkYulCat'LVM g) =
                          (uncurryingNP
                            @g @xs @(P'V vn r b)
                            @(P'V v1 r) @(YulMonad v1 vn r) @(YulCat'LVV v1 v1 r a) @(YulCat'LVM v1 vn r a) @One
                            (f x)
                            (MkYulCat'LVV (\a -> ignore (discard a) xs))
                          )
                    in g xxs2
    )

yulmonad'lv :: forall f xs b f1 b1 r vd m1 m1b m2 m2b.
               ( YulO3 (NP xs) b r
               , UncurryNP'Fst f ~ xs
               , UncurryNP'Snd f ~ b
               , P'V vd r b ~ b1
               , CurryNP (NP xs) b1 ~ f1 -- this is to workaround @m1b cannot lift @b@ to a port
               , UncurryNP'Fst f1 ~ xs
               , UncurryNP'Snd f1 ~ b1
               , P'V         0 r ~ m1
               , YulMonad 0 vd r ~ m1b
               , YulCat'LVV 0  0 r (NP xs) ~ m2
               , YulCat'LVM 0 vd r (NP xs) ~ m2b
               , UncurryingNP f1 xs b1 m1 m1b m2 m2b One
               , LiftFunction b1 m2 m2b One ~ m2b b1
               )
            => LiftFunction f1 m1 m1b One      -- ^ LiftFunction               f1 m1 m1b One
            -> (P'V 0 r (NP xs) ⊸ P'V vd r b) -- ^ LiftFunction (NP (():xs) -> b) m1 m1b One
yulmonad'lv f = let !(MkYulCat'LVM f') = uncurryingNP @f1 @xs @b1 @m1 @m1b @m2 @m2b @One f (MkYulCat'LVV id)
  in \xs -> mkUnit xs
  & \(xs', u) -> runYulMonad u (f' xs')


--
-- yulmonad'lp
--

-- | Monadic yul port diagrams for pure input and yul monad output.
newtype YulCat'LPM v1 vn r a b = MkYulCat'LPM (P'P r a ⊸ YulMonad v1 vn r b)

instance forall x v1 vn r a.
         ( YulO3 x r a
         , LiftFunction x (P'P r) (P'V vn r) One ~ P'V vn r x
         ) => UncurryingNP (P'V vn r x) '[] (P'V vn r x)
         (P'P r) (YulMonad v1 vn r)
         (YulCat'LPP r a) (YulCat'LPM v1 vn r a) One where
  uncurryingNP x (MkYulCat'LPP h) = MkYulCat'LPM
    \a -> toss (UnsafeLinear.coerce @_ @(P'V v1 r x) (h a & coerce'l @_ @())) LVM.>> x

instance forall x xs b g v1 vn r a.
         ( YulO5 x (NP xs) b r a
         , UncurryingNP g xs (P'V vn r b) (P'P r) (YulMonad v1 vn r) (YulCat'LPP r a) (YulCat'LPM v1 vn r a) One
         ) => UncurryingNP (x -> g) (x:xs) (P'V vn r b)
         (P'P r) (YulMonad v1 vn r)
         (YulCat'LPP r a) (YulCat'LPM v1 vn r a) One where
  uncurryingNP f (MkYulCat'LPP h) = MkYulCat'LPM
    (\xxs ->
        dup2'l xxs
      & \(xxs1, xxs2) -> split (coerce'l @(NP (x:xs)) @(x, NP xs) (h xxs1))
      & \(x, xs) -> let !(MkYulCat'LPM g) =
                          (uncurryingNP
                            @g @xs @(P'V vn r b)
                            @(P'P r) @(YulMonad v1 vn r) @(YulCat'LPP r a) @(YulCat'LPM v1 vn r a) @One
                            (f x)
                            (MkYulCat'LPP (\a -> ignore (discard a) xs))
                          )
                    in g xxs2
    )

yulmonad'lp :: forall f xs b f1 b1 r vd m1 m1b m2 m2b.
               ( YulO3 (NP xs) b r
               , UncurryNP'Fst f ~ xs
               , UncurryNP'Snd f ~ b
               , P'V vd r b ~ b1
               , CurryNP (NP xs) b1 ~ f1 -- this is to workaround @m1b cannot lift @b@ to a port
               , UncurryNP'Fst f1 ~ xs
               , UncurryNP'Snd f1 ~ b1
               , P'P           r ~ m1
               , YulMonad 0 vd r ~ m1b
               , YulCat'LPP      r (NP xs) ~ m2
               , YulCat'LPM 0 vd r (NP xs) ~ m2b
               , UncurryingNP f1 xs b1 m1 m1b m2 m2b One
               , LiftFunction b1 m2 m2b One ~ m2b b1
               )
            => LiftFunction f1 m1 m1b One   -- ^ LiftFunction               f1 m1 m1b One
            -> (P'P r (NP xs) ⊸ P'V vd r b) -- ^ LiftFunction (NP (():xs) -> b) m1 m1b One
yulmonad'lp f = let !(MkYulCat'LPM f') = uncurryingNP @f1 @xs @b1 @m1 @m1b @m2 @m2b @One f (MkYulCat'LPP id)
  in \xs -> mkUnit xs
  & \(xs', u) -> runYulMonad u (f' xs')

--
-- call'l
--

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
       (\(MkYulCat'LVV fxs) -> encode'lvv (YulJmp (UserDefinedYulCat (fnId f, fnCat f))) (cons'l x' (fxs (discard x''))))

--
-- pattern matching
--

match'l :: forall p r a b va vd.
           ( YulO4 r a b (p a)
           , BasePrelude.Functor p
           , PatternMatchable p a)
        => P'V va r (p a)
        ⊸ (forall r1. YulO1 r1 => p (P'V va r1 (p a) ⊸ P'V va r1 a) ⊸ (P'V 0 r1 (p a) ⊸ P'V vd r1 b))
        -> P'V (va + vd) r b
match'l p f = let c = match (YulId :: YulCat (VersionedInputOutput 0) (p a) (p a))
                      \cs -> UnsafeLinear.coerce
                             @(YulCat (VersionedInputOutput vd) (p a) b)
                             @(YulCat (VersionedInputOutput 0) (p a) b)
                             (decode'lvv (f (BasePrelude.fmap encode'lvv cs)))
                  c' = UnsafeLinear.coerce
                       @(YulCat (VersionedInputOutput 0) (p a) b)
                       @(YulCat (VersionedInputOutput vd) (p a) b)
                       c
              in encode'lvv c' p
