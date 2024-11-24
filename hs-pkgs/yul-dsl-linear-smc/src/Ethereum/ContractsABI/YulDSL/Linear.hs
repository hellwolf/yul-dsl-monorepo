{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE NoImplicitPrelude   #-}

module Ethereum.ContractsABI.YulDSL.Linear where


-- base
import           GHC.TypeLits                                  (type (+))
-- linear-base
import           Prelude.Linear
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


{- * Yul Port Types -}

-- | Linearized effect, where @v@ is a type-level version of the data.
data LinearEffect = MkLinearEffect Nat

-- | Linear port API of yul category with `LinearEffect` kind.
type P'L v r = P (YulCat (MkLinearEffect v)) r

-- | Polymorphic port type for linear APIs of the yul category.
-- type P'L v r a = P'L v r a

type UNIT'L v r = P'L v r ()
type ADDR'L v r = P'L v r ADDR
type BOOL'L v r = P'L v r BOOL
type I256'L v r = P'L v r I256
type U256'L v r = P'L v r U256

-- | Yul category port diagram as a data constructor, otherwise type synonym cannot be partial for @YulCat'L r a@.
data YulCat'L v1 vn r a b where
  MkYulCat'L :: forall a b r v1 vn. YulO3 a b r => (P'L v1 r a ⊸ P'L vn r b) ⊸ YulCat'L v1 vn r a b

-- | Unwrap YulCat'L linearly.
unYulCat'L :: forall a b r v1 vn. YulO3 a b r => YulCat'L v1 vn r a b ⊸ (P'L v1 r a ⊸ P'L vn r b)
unYulCat'L (MkYulCat'L c) =  c

{- * Yul Port Combinators -}

dis'l :: forall d v r. YulO2 d r
      => P'L v r d ⊸ P'L v r ()
dis'l = discard

const'l :: forall a d v r. YulO3 a d r
        => a -> (P'L v r d ⊸ P'L v r a)
const'l a = encode (YulEmbed a) . discard

coerce'l :: forall a b v r. (YulO3 a b r, ABITypeCoercible a b)
         => P'L v r a ⊸ P'L v r b
coerce'l = encode YulCoerce

dup2'l :: forall a v r. YulO2 a r
       => P'L v r a ⊸ (P'L v r a, P'L v r a)
dup2'l = split . copy

cons'l :: forall x xs v r. YulO3 x (NP xs) r
         => P'L v r x ⊸ P'L v r (NP xs) ⊸ P'L v r (NP (x:xs))
cons'l x xs = coerce'l (merge (x, xs))

{- * Ethereum.ContractABI instances * -}

{- ** UncurryingNP instances -}

instance forall x v1 vn r a.
         ( LiftFunction x (P'L v1 r) (P'L vn r) One ~ P'L vn r x
         , YulO3 x r a
         , x ~ UncurryNP'Snd x
         ) => UncurryingNP (x) '[] x (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r a) (YulCat'L v1 vn r a) One where
  uncurryingNP x (MkYulCat'L g) = MkYulCat'L (\a -> g a &                 -- :: P'L v1 (NP '[])
                                                    coerce'l @_ @() &     -- :: P'L v1 ()
                                                    UnsafeLinear.coerce & -- :: P'L vn ()
                                                    \u -> ignore u x)     -- :: P'L vn x

instance forall x xs b g v1 vn r a.
         ( YulO5 x (NP xs) b r a
         , UncurryingNP g xs b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r a) (YulCat'L v1 vn r a) One
         ) => UncurryingNP (x -> g) (x:xs) b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r a) (YulCat'L v1 vn r a) One where
  uncurryingNP f (MkYulCat'L h) = MkYulCat'L
    (\xxs -> dup2'l xxs &
             \(xxs1, xxs2) -> split (coerce'l @(NP (x:xs)) @(x, NP xs) (h xxs1)) &
             \(x, xs) -> unYulCat'L
                         (uncurryingNP
                           @g @xs @b
                           @(P'L v1 r) @(P'L vn r) @(YulCat'L v1 v1 r a) @(YulCat'L v1 vn r a) @One
                           (UnsafeLinear.coerce f x) -- TODO: not sure why this unsafe coercion is required.
                           (g xs)
                         )
                         xxs2
    )
    where g :: P'L v1 r (NP xs) ⊸ YulCat'L v1 v1 r a (NP xs)
          g xs = MkYulCat'L (\as -> ignore (discard as) xs)

{- ** CurryingNP instances -}

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

uncurry'l :: forall f xs b r vd f'.
             ( YulO3 (NP xs) b r
             , xs ~ UncurryNP'Fst f
             , b  ~ UncurryNP'Snd f
             , f' ~ LiftFunction f (P'L 0 r) (P'L vd r) One
             , UncurryingNP f xs b (P'L 0 r) (P'L vd r) (YulCat'L 0 0 r (NP xs)) (YulCat'L 0 vd r (NP xs)) One
             )
          => f'
          -> (P'L 0 r (NP xs) ⊸ P'L vd r b)
uncurry'l f = unYulCat'L (uncurryingNP
                           @f @xs @b
                           @(P'L 0 r) @(P'L vd r) @(YulCat'L 0 0 r (NP xs)) @(YulCat'L 0 vd r (NP xs)) @One
                           f (MkYulCat'L id))

-- | Define a `YulCat` morphism from a linear port function.
fn'l :: forall f xs b vd.
        ( YulO2 (NP xs) b
        , CurryNP (NP xs) b ~ f
        , UncurryNP'Fst f ~ xs
        , UncurryNP'Snd f ~ b
        )
     => String
     -> (forall r. YulObj r => P'L 0 r (NP xs) ⊸ P'L vd r b)
     -> Fn (MkLinearEffect vd) (CurryNP (NP xs) b)
fn'l fid f = MkFn (MkFnCat fid (decode (h f)))
  where h :: (forall r. YulObj r => P'L 0 r (NP xs) ⊸ P'L vd r b)
          ⊸ (forall r. YulObj r => P'L vd r (NP xs) ⊸ P'L vd r b)
        h = UnsafeLinear.coerce

call'l :: forall f x xs b g' r v1 vd vn.
          ( YulO4 x (NP xs) b r
          , UncurryNP'Fst f ~ (x:xs)
          , UncurryNP'Snd f ~ b
          , v1 + vd ~ vn
          , LiftFunction (CurryNP (NP xs) b) (P'L v1 r) (P'L vn r) One ~ g'
          , CurryingNP xs b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r ()) One
          )
       => Fn (MkLinearEffect vd) f
       -> (P'L v1 r x ⊸ g')
call'l (MkFn f) x' = dup2'l x' &
  \(x'', x''') ->
    curryingNP @xs @b @(P'L v1 r) @(P'L vn r) @(YulCat'L v1 v1 r ()) @One
    (\(MkYulCat'L fxs) -> g x'' (fxs (discard x''')))
  where cat :: YulCat (MkLinearEffect v1) (NP (x:xs)) b
        cat = UnsafeLinear.coerce (fnCat f)
        g :: forall. P'L v1 r x ⊸ P'L v1 r (NP xs) ⊸ P'L vn r b
        g x xs = UnsafeLinear.coerce (encode (YulJump (fnId f) cat) (cons'l x xs))

{- * storage utilities -}

sget :: forall a r v. (YulO2 a r, ABIWordValue a)
     => P'L v r ADDR ⊸ P'L v r a
sget = encode YulSGet

sput :: forall a r v. (YulO2 a r, ABIWordValue a)
     => P'L v r ADDR ⊸ P'L v r a ⊸ P'L (v + 1) r a
sput to x = dup2'l x &
            \(x', x'') -> encode YulSPut (merge (to, x')) &
            \u -> UnsafeLinear.coerce (ignore u x'')

sputAt :: forall a r v. (YulO2 a r, ABIWordValue a)
       => ADDR -> P'L v r a ⊸ P'L (v + 1) r a
sputAt to x = mkUnit x & \(x', u) -> const'l to u & \a -> sput a x'
