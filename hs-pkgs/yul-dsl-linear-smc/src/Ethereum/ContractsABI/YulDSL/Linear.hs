{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude   #-}

module Ethereum.ContractsABI.YulDSL.Linear where


-- base
import           GHC.TypeLits                                  (type (+), type (-))
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

-- | Linear port API with `LinearEffect` tag.
type P'L v = P (YulCat (MkLinearEffect v))

-- | Polymorphic port type for linear APIs of the yul category.
-- type P'L v r a = P'L v r a

type UNIT'L v r = P'L v r ()
type ADDR'L v r = P'L v r ADDR
type BOOL'L v r = P'L v r BOOL
type U256'L v r = P'L v r U256
type I256'L v r = P'L v r I256

-- | Yul category port diagram as a data constructor, otherwise type synonym cannot be partial for @YulCat'L r a@.
data YulCat'L v1 vn r a b where
  MkYulCat'L :: forall a b r v1 vn. YulO3 a b r => (P'L v1 r a ⊸ P'L vn r b) ⊸ YulCat'L v1 vn r a b

-- MkYulCat'L :: forall a b v vd r.  (P'L v r a ⊸ P'L (v + vd) r b) ⊸ YulCat'L vd r a b
-- MkYulCat'L = MkYulCat'L . UnsafeLinear.coerce {- DON'T DO THIS AT HOME -}

-- | Unwrap YulCat'L linearly.
unYulCat'L :: forall a b r v1 vn. YulO3 a b r => YulCat'L v1 vn r a b ⊸ (P'L v1 r a ⊸ P'L vn r b)
unYulCat'L (MkYulCat'L c) =  c

{- * Yul Port Combinators -}

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

instance forall x v r a.
         ( LiftFunction x (P'L v r) (P'L v r) One ~ P'L v r x
         , YulO3 x r a
         , x ~ UncurryNP'Snd x
         ) => UncurryingNP (x) '[] x (P'L v r) (P'L v r) (YulCat'L v v r a) (YulCat'L v v r a) One where
  uncurryingNP x (MkYulCat'L g) = MkYulCat'L (\a -> ignore (coerce'l (g a)) x)

instance forall x xs b g v1 vn r a.
         ( YulO5 x (NP xs) b r a
         , UncurryingNP g xs b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r a) (YulCat'L v1 vn r a) One
         ) => UncurryingNP (x -> g) (x:xs) b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r a) (YulCat'L v1 vn r a) One where
  uncurryingNP f (MkYulCat'L g) = MkYulCat'L
    (\xxs -> dup2'l xxs &
             \(xxs1, xxs2) -> split (coerce'l (g xxs1)) &
             \(x, xs) -> unYulCat'L
                         (uncurryingNP
                           @g @xs @b @(P'L v1 r) @(P'L vn r) @(YulCat'L v1 v1 r a) @(YulCat'L v1 vn r a) @One
                           (f x) (g' xs)
                         )
                         xxs2
    )
    where g' :: P'L v1 r (NP xs) ⊸ YulCat'L v1 v1 r a (NP xs)
          g' xs = MkYulCat'L (\as -> ignore (discard as) xs)

{- ** CurryingNP instances -}

instance forall x v r a.
         ( YulO3 x r a
         , LiftFunction (CurryNP (NP '[]) x) (P'L v r) (P'L v r) One ~ P'L v r x
         ) => CurryingNP '[] x (P'L v r) (P'L v r) (YulCat'L v v r a) One where
  curryingNP cb = cb (MkYulCat'L (\a -> coerce'l (discard a)))

instance forall x xs b r a v1 vn.
         ( YulO5 x (NP xs) b r a
         , CurryingNP xs b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r a) One
         ) => CurryingNP (x:xs) b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r a) One where
  curryingNP cb x = curryingNP @xs @b @(P'L v1 r) @(P'L vn r) @(YulCat'L v1 v1 r a) @One
                    (\(MkYulCat'L fxs) -> cb (MkYulCat'L (\a -> (cons'l x (fxs a)))))

curry'l :: forall f as b r v1 vn f'.
        ( YulO3 (NP as) b r
        , as ~ UncurryNP'Fst f
        , b  ~ UncurryNP'Snd f
        , f' ~ LiftFunction f (P'L v1 r) (P'L vn r) One
        , UncurryingNP f as b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r (NP as)) (YulCat'L v1 vn r (NP as)) One
        ) => f' -> (P'L v1 r (NP as) ⊸ P'L vn r b)
curry'l f' = unYulCat'L (uncurryingNP
                          @f @as @b @(P'L v1 r) @(P'L vn r) @(YulCat'L v1 v1 r (NP as)) @(YulCat'L v1 vn r (NP as)) @One
                         f' (MkYulCat'L id))

-- | Define a `YulCat` morphism from a linear port function.
fn'l :: forall xs b v1 vd vn.
        ( YulO2 (NP xs) b
        , UncurryNP'Fst (CurryNP (NP xs) b) ~ xs
        , UncurryNP'Snd (CurryNP (NP xs) b) ~ b
        , v1 + vd ~ vn
        )
     => String
     -> (forall r. YulO1 r => P'L v1 r (NP xs) ⊸ P'L vn r b)
     -> Fn (MkLinearEffect vd) (CurryNP (NP xs) b)
fn'l fid f = fn
  where cat :: forall. YulCat (MkLinearEffect vd) (NP xs) b
        cat = UnsafeLinear.coerce (decode f)
        fn :: Fn (MkLinearEffect vd) (CurryNP (NP xs) b)
        fn = MkFn (MkFnCat fid cat)

call'l :: forall f x xs b g' r v1 vd vn.
        ( YulO4 x (NP xs) b r
        , UncurryNP'Fst f ~ (x:xs)
        , UncurryNP'Snd f ~ b
        , v1 + vd ~ vn
        , LiftFunction (CurryNP (NP xs) b) (P'L v1 r) (P'L vn r) One ~ g'
        , CurryingNP xs b (P'L v1 r) (P'L vn r) (YulCat'L v1 v1 r ()) One
        )
     => Fn (MkLinearEffect vd) f -> (P'L v1 r x ⊸ g')
call'l (MkFn f) x' = dup2'l x' &
  \(x'', x''') ->
    curryingNP @xs @b @(P'L v1 r) @(P'L vn r) @(YulCat'L v1 v1 r ()) @One
    (\(MkYulCat'L fxs) -> g x'' (fxs (discard x''')))
  where cat :: YulCat (MkLinearEffect v1) (NP (x:xs)) b
        cat = UnsafeLinear.coerce (fnCat f)
        g :: forall. P'L v1 r x ⊸ P'L v1 r (NP xs) ⊸ P'L vn r b
        g x xs = UnsafeLinear.coerce (encode (YulJump (fnId f) cat) (cons'l x xs))

{- * storage utilities -}

-- sget :: forall v r. (YulO2 v r, ABIWordValue v)
--      => P'L r ADDR ⊸ P'L r v -- FIXME use Maybe type
-- sget = encode YulSGet

-- sput :: forall v r. (YulO2 v r, ABIWordValue v)
--      => P'L r ADDR ⊸ P'L r v ⊸ P'L r ()
-- sput toP valP = encode YulSPut (merge (toP, valP))
-- (<==) :: forall v r. (YulO2 v r, ABIWordValue v)
--       => P'L r ADDR ⊸ P'L r v ⊸ P'L r ()
-- (<==) = sput

-- sputAt :: forall v r. (YulO2 v r, ABIWordValue v)
--        => ADDR -> P'L r v ⊸ P'L r ()
-- sputAt to v = mkUnit v & \(v', u) -> const'l to u & \a -> sput a v'
-- (<==@) :: forall v r. (YulO2 v r, ABIWordValue v)
--       => ADDR -> P'L r v ⊸ P'L r ()
-- (<==@) = sputAt

-- infixr 1 <==, <==@
