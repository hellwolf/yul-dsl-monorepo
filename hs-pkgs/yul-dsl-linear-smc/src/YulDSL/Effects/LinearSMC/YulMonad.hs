module YulDSL.Effects.LinearSMC.YulMonad
  ( YulMonad, runYulMonad
  -- * LVM Combinators
  , module Control.LinearlyVersionedMonad.Combinators
  , Control.Functor.Linear.fmap
  -- * YulMonad Combinator
  -- ** Port Purity
  , Impurable (impure), impureN
  -- ** Storages
  , sget, sput, sput_, sputAt
  ) where

-- base
import           GHC.TypeLits                               (type (+))
-- constraints
import           Data.Constraint.Unsafe                     (unsafeAxiom)
-- linear-base
import           Control.Category.Linear                    (discard, encode, ignore, merge, mkUnit)
import qualified Control.Functor.Linear
import           Prelude.Linear                             ((&))
import qualified Unsafe.Linear                              as UnsafeLinear
-- yul-dsl
import           YulDSL.Core
--
import           Control.LinearlyVersionedMonad             (LVM (..), runLVM)
import qualified Control.LinearlyVersionedMonad             as LVM
import           Control.LinearlyVersionedMonad.Combinators
import           Data.LinearContext
--
import           YulDSL.Effects.LinearSMC.YulPort


--------------------------------------------------------------------------------
-- YulMonad: A Linearly Versioned Monad for YulDSL
--------------------------------------------------------------------------------

-- | YulMonad is a linear versioned monad with 'YulMonadCtx' as its context data.
type YulMonad va vb r = LVM (YulMonadCtx r) va vb

runYulMonad :: forall vd r a ue . YulO2 r a
            => P'x ue r () ⊸ YulMonad 0 vd r (P'V vd r a) ⊸ P'V vd r a
runYulMonad u m = let !(ctx', a) = runLVM (MkYulMonadCtx (UnsafeLinear.coerce u)) m
                      !(MkYulMonadCtx (MkUnitDumpster u')) = ctx'
                  in ignore (UnsafeLinear.coerce u') a

--------------------------------------------------------------------------------
-- Impurable (impure)
--------------------------------------------------------------------------------

class Impurable x'p x'v where
  -- | Safe operation that impures a pure yul port to a versioned yul port.
  impure :: x'p ⊸ YulMonad v v r x'v

instance forall v r a. Impurable (P'P r a) (P'V v r a) where
  impure x = pure (UnsafeLinear.coerce x)

instance Impurable (NP '[]) (NP '[]) where
  impure Nil = pure Nil

instance forall x'p x'v xs'p xs'v.
         ( Impurable x'p x'v, Impurable (NP xs'p) (NP xs'v)
         ) => Impurable (NP (x'p:xs'p)) (NP (x'v:xs'v)) where
  impure (x :* xs) = LVM.do
    x' <- impure x
    xs' <- impure xs
    pure (x' :* xs')

impureN :: forall v r tpl'p tpl'v.
           ( ConvertibleTupleN tpl'p
           , ConvertibleTupleN tpl'v
           -- , TupleNtoNP tpl'p ~ NP (P'P r a:xs)
           -- , TupleNtoNP tpl'v ~ NP (P'V v r a:xs)
           , Impurable (TupleNtoNP tpl'p) (TupleNtoNP tpl'v)
           )
        => tpl'p ⊸ YulMonad v v r tpl'v
impureN tpl'p = LVM.do
  np'v :: TupleNtoNP tpl'v <- impure (fromTupleNtoNP tpl'p)
  pure (fromNPtoTupleN np'v)

--------------------------------------------------------------------------------
-- Storage Combinators
--------------------------------------------------------------------------------

sget :: forall v r a. (YulO2 r a, ABIWordValue a)
     => P'V v r ADDR ⊸ YulMonad v v r (P'V v r a)
sget a = pure (encode YulSGet a)

sput :: forall v r a. (YulO2 r a, ABIWordValue a)
     => P'V v r ADDR ⊸ P'V v r a ⊸ YulMonad v (v + 1) r (P'V (v + 1) r a)
sput to x =
  dup2'l x
  & \(x1, x2) -> encode YulSPut (merge (to, x1))
  & \u -> MkLVM (unsafeAxiom, , UnsafeLinear.coerce (ignore u x2))

sput_ :: forall v r a. (YulO2 r a, ABIWordValue a)
      => P'V v r ADDR ⊸ P'V v r a ⊸ YulMonad v (v + 1) r (P'V (v + 1) r ())
sput_ to x =
  encode YulSPut (merge (to, x))
  & \u -> MkLVM (unsafeAxiom, , UnsafeLinear.coerce u)

sputAt :: forall v r a. (YulO2 r a, ABIWordValue a)
       => ADDR ⊸ P'V v r a ⊸ YulMonad v (v + 1) r (P'V (v + 1) r a)
sputAt to x = mkUnit x & \(x', u) -> emb'l to u & \a -> sput a x'

--------------------------------------------------------------------------------
-- (Internal Stuff)
--------------------------------------------------------------------------------

-- A dumpster of unitals.
newtype UnitDumpster r = MkUnitDumpster (P'V 0 r ())

-- Duplicate a unit.
ud_udup :: forall eff r. YulO1 r
        => UnitDumpster r ⊸ (UnitDumpster r, P'x eff r ())
ud_udup (MkUnitDumpster u) = let !(u1, u2) = dup2'l u in (MkUnitDumpster u1, UnsafeLinear.coerce u2)

-- Gulp an input port.
ud_gulp :: forall eff r a. YulO2 r a
        => P'x eff r a ⊸ UnitDumpster r ⊸ UnitDumpster r
ud_gulp x (MkUnitDumpster u) = let u' = ignore (UnsafeLinear.coerce (discard x)) u
                               in MkUnitDumpster u'

-- Copy an input port.
-- ud_copy :: forall eff r a. YulO2 r a
--         => (P'x eff r () ⊸ P'x eff r a) ⊸ UnitDumpster r ⊸ (UnitDumpster r, P'x eff r a)
-- ud_copy f (MkUnitDumpster u) = let !(u1, u2) = dup2'l u
--                                    x = f (UnsafeLinear.coerce u1)
--                                in (MkUnitDumpster u2, x)

-- Context to be with the 'YulMonad'.
newtype YulMonadCtx r = MkYulMonadCtx (UnitDumpster r)

instance YulO2 r a => ContextualConsumable (YulMonadCtx r) (P'x eff r a) where
  contextualConsume (MkYulMonadCtx ud) x = MkYulMonadCtx (ud_gulp x ud)

instance YulO2 r a => ContextualDupable (YulMonadCtx r) (P'x eff r a) where
  contextualDup ctx x = (ctx, dup2'l x)

instance YulO2 r a => ContextualEmbeddable (YulMonadCtx r) (P'V v r) a where
  contextualEmbed (MkYulMonadCtx ud) x'p = let !(ud', u') = ud_udup ud
                                               x'v = emb'l x'p u'
                                           in (MkYulMonadCtx ud', x'v)
