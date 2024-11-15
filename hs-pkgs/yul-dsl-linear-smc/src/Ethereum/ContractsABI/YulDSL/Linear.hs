{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude   #-}

module Ethereum.ContractsABI.YulDSL.Linear where

-- linear-base
import           Prelude.Linear
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

-- | Polymorphic port type for linear function APIs of YulDSL
type Yul'P r a = P YulCat r a

type UNIT'P r = Yul'P r ()
type ADDR'P r = Yul'P r ADDR
type BOOL'P r = Yul'P r BOOL
type U256'P r = Yul'P r U256
type I256'P r = Yul'P r I256

-- | Yul category port diagram as a data constructor, otherwise type synonym cannot be partial for @YulCat'P r a@.
data YulCat'P r a b where
  MkYulCat'P :: forall a b r. (Yul'P r a ⊸ Yul'P r b) ⊸ YulCat'P r a b

-- | Unwrap YulCat'P linearly.
unYulCat'P :: forall a b r. YulCat'P r a b ⊸ (Yul'P r a ⊸ Yul'P r b)
unYulCat'P (MkYulCat'P c) = c

{- * Yul Port Combinators -}

const'l :: forall a d r. YulO3 a d r
        => a -> (Yul'P r d ⊸ Yul'P r a)
const'l a = encode (YulEmbed a) . discard

coerce'l :: forall a b r. (YulO3 a b r, ABITypeCoercible a b)
         => Yul'P r a ⊸ Yul'P r b
coerce'l = encode YulCoerce

dup2'l :: forall a r. YulO2 a r
       => Yul'P r a ⊸ (Yul'P r a, Yul'P r a)
dup2'l = split . copy

cons'l :: forall x xs r. YulO3 x (NP xs) r
         => Yul'P r x ⊸ Yul'P r (NP xs) ⊸ Yul'P r (NP (x:xs))
cons'l x xs = coerce'l (merge (x, xs))

{- * Ethereum.ContractABI instances * -}

{- ** UncurryingNP instances -}

instance forall x r a.
         ( LiftFunction x (P YulCat r) One ~ P YulCat r x
         , YulO3 x r a
         , x ~ UncurryNP'Snd x
         ) => UncurryingNP (x) '[] x (P YulCat r) (YulCat'P r a) One where
  uncurryingNP x (MkYulCat'P g) = MkYulCat'P (\a -> ignore (coerce'l (g a)) x)

instance forall x xs b g r a.
         ( YulO5 x (NP xs) b r a
         , UncurryingNP g xs b (P YulCat r) (YulCat'P r a) One
         ) => UncurryingNP (x -> g) (x:xs) b (P YulCat r) (YulCat'P r a) One where
  uncurryingNP f (MkYulCat'P g) = MkYulCat'P
    (\xxs -> dup2'l xxs &
             \(xxs1, xxs2) -> split (coerce'l (g xxs1)) &
             \(x, xs) -> unYulCat'P
                         ( uncurryingNP @g @xs @b @(P YulCat r) @(YulCat'P r a) @One
                           (f x) (g' xs))
                         xxs2
    )
    where g' :: Yul'P r (NP xs) ⊸ YulCat'P r a (NP xs)
          g' xs = MkYulCat'P (\as -> ignore (discard as) xs)

{- ** CurryingNP instances -}

instance forall x r a.
         ( YulO3 x r a
         , LiftFunction (CurryNP (NP '[]) x) (P YulCat r) One ~ P YulCat r x
         ) => CurryingNP '[] x (P YulCat r) (YulCat'P r a) One where
  curryingNP cb = cb (MkYulCat'P (\a -> coerce'l (discard a)))

instance forall x xs b r a.
         ( YulO5 x (NP xs) b r a
         , CurryingNP xs b (P YulCat r) (YulCat'P r a) One
         ) => CurryingNP (x:xs) b (P YulCat r) (YulCat'P r a) One where
  curryingNP cb x = curryingNP @xs @b @(P YulCat r) @(YulCat'P r a) @One
                    (\(MkYulCat'P fxs) -> cb (MkYulCat'P (\a -> (cons'l x (fxs a)))))

curry'l :: forall f as b r f'.
        ( YulO3 (NP as) b r
        , as ~ UncurryNP'Fst f
        , b  ~ UncurryNP'Snd f
        , f' ~ LiftFunction f (P YulCat r) One
        , UncurryingNP f as b (P YulCat r) (YulCat'P r (NP as)) One
        ) => f' -> (Yul'P r (NP as) ⊸ Yul'P r b)
curry'l f' = unYulCat'P (uncurryingNP @f @as @b @(P YulCat r) @(YulCat'P r (NP as)) @One
                         f' (MkYulCat'P id))

-- | Define a `YulCat` morphism from a linear port function.
fn'l :: forall as b.
        ( YulO2 (NP as) b
        , UncurryNP'Fst (CurryNP (NP as) b) ~ as
        , UncurryNP'Snd (CurryNP (NP as) b) ~ b
        )
     => String
     -> (forall r. YulO1 r => Yul'P r (NP as) ⊸ Yul'P r b)
     -> Fn (CurryNP (NP as) b)
fn'l fid cat'l = MkFn (MkFnCat fid (decode cat'l))

call'l :: forall f x xs b g' r.
        ( YulO4 x (NP xs) b r
        , UncurryNP'Fst f ~ (x:xs)
        , UncurryNP'Snd f ~ b
        , LiftFunction (CurryNP (NP xs) b) (P YulCat r) One ~ g'
        , CurryingNP xs b (P YulCat r) (YulCat'P r ()) One
        )
     => Fn f -> (Yul'P r x ⊸ g')
call'l (MkFn f) x' = dup2'l x' &
  \(x'', x''') ->
    curryingNP @xs @b @(P YulCat r) @(YulCat'P r ()) @One
    (\(MkYulCat'P fxs) -> g x'' (fxs (discard x''')))
  where g :: Yul'P r x ⊸ Yul'P r (NP xs) ⊸ Yul'P r b
        g x xs = encode (YulJump (fnId f) (fnCat f)) (cons'l x xs)

{- * storage utilities -}

sget :: forall v r. (YulObj r, YulVal v)
     => Yul'P r ADDR ⊸ Yul'P r (Maybe v)
sget = encode YulSGet

sput :: forall v r. (YulObj r, YulVal v)
     => Yul'P r ADDR ⊸ Yul'P r v ⊸ Yul'P r ()
sput toP valP = encode YulSPut (merge (toP, valP))
(<==) :: forall v r. (YulObj r, YulVal v)
      => Yul'P r ADDR ⊸ Yul'P r v ⊸ Yul'P r ()
(<==) = sput

sputAt :: forall v r. (YulObj r, YulVal v)
       => ADDR -> Yul'P r v ⊸ Yul'P r ()
sputAt to v = mkUnit v & \(v', u) -> const'l to u & \a -> sput a v'
(<==@) :: forall v r. (YulObj r, YulVal v)
      => ADDR -> Yul'P r v ⊸ Yul'P r ()
(<==@) = sputAt
infixr 1 <==, <==@
