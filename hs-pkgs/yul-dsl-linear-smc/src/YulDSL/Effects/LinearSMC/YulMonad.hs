{-# LANGUAGE DerivingVia #-}
module YulDSL.Effects.LinearSMC.YulMonad
  ( YulMonad, runYulMonad
  , pure, with, pass, lift, toss, embed
  , Control.Functor.Linear.fmap
  , sget, sput, sput_, sputAt
  ) where

-- base
import           GHC.TypeLits                     (type (+))
-- constraints
import           Data.Constraint                  (Dict (Dict))
import           Data.Constraint.Unsafe           (unsafeAxiom)
-- linear-base
import           Control.Category.Linear          (discard, encode, ignore, merge, mkUnit)
import qualified Control.Functor.Linear
import           Prelude.Linear                   (lseq, (&))
import qualified Unsafe.Linear                    as UnsafeLinear
-- yul-dsl
import           YulDSL.Core
--
import           Control.LinearlyVersionedMonad   (LVM (..), pass, pure, runLVM, with)
import           Data.LinearContext
--
import           YulDSL.Effects.LinearSMC.YulPort

-- | YulMonad is a linear versioned monad with 'YulMonadCtx' as its context data.
type YulMonad va vb r = LVM (YulMonadCtx r) va vb

runYulMonad :: forall vd r a ue . YulO2 r a
            => P'x ue r () ⊸ YulMonad 0 vd r (P'V vd r a) ⊸ P'V vd r a
runYulMonad u m = let !(ctx', a) = runLVM (MkYulMonadCtx (UnsafeLinear.coerce u)) m
                      !(MkYulMonadCtx (MkUnitDumpster u')) = ctx'
                  in ignore (UnsafeLinear.coerce u') a

-- | Safe operation that lifts a pure yul port to a versioned yul port.
lift :: forall v r a. YulO2 r a
       => P'P r a ⊸ YulMonad v v r (P'V v r a)
lift x = MkLVM \ctx -> (Dict, ctx, UnsafeLinear.coerce x)

toss :: forall v r a. YulO2 r a
       => P'V v r a ⊸ YulMonad v v r (P'V v r ())
toss x = MkLVM \ctx -> (Dict, ctx, discard x)

embed :: forall v r a. YulO2 r a
       => a ⊸ YulMonad v v r (P'V v r a)
embed x = MkLVM \(MkYulMonadCtx ud) -> let f = emb'l x
                                           !(ud', x') = ud_copy f ud
                                       in (Dict, MkYulMonadCtx ud', x')

sget :: forall v r a. (YulO2 r a, ABIWordValue a)
     => P'V v r ADDR ⊸ YulMonad v v r (P'V v r a)
sget a = MkLVM \ctx -> (Dict, ctx, encode YulSGet a)

sput :: forall v r a. (YulO2 r a, ABIWordValue a)
     => P'V v r ADDR ⊸ P'V v r a ⊸ YulMonad v (v + 1) r (P'V (v + 1) r a)
sput to x =
  dup2'l x
  & \(x1, x2) -> encode YulSPut (merge (to, x1))
  & \u -> MkLVM \ctx -> (unsafeAxiom, ctx, UnsafeLinear.coerce (ignore u x2))

sput_ :: forall v r a. (YulO2 r a, ABIWordValue a)
      => P'V v r ADDR ⊸ P'V v r a ⊸ YulMonad v (v + 1) r (P'V (v + 1) r ())
sput_ to x =
  encode YulSPut (merge (to, x))
  & \u -> MkLVM \ctx -> (unsafeAxiom, ctx, UnsafeLinear.coerce u)

sputAt :: forall v r a. (YulO2 r a, ABIWordValue a)
       => ADDR ⊸ P'V v r a ⊸ YulMonad v (v + 1) r (P'V (v + 1) r a)
sputAt to x = mkUnit x & \(x', u) -> emb'l to u & \a -> sput a x'

--
-- Internal Stuff
--

-- | A dumpster of unitals.
newtype UnitDumpster r = MkUnitDumpster (P'V 0 r ())

ud_copy :: forall eff r a. YulO2 r a
        => (P'x eff r () ⊸ P'x eff r a) ⊸ UnitDumpster r ⊸ (UnitDumpster r, P'x eff r a)
ud_copy f (MkUnitDumpster u) = let !(u1, u2) = dup2'l u
                                   x = f (UnsafeLinear.coerce u1)
                               in (MkUnitDumpster u2, x)

ud_gulp :: forall eff r a. YulO2 r a
        => P'x eff r a ⊸ UnitDumpster r ⊸ UnitDumpster r
ud_gulp x (MkUnitDumpster u) = let u' = ignore (UnsafeLinear.coerce (discard x)) u
                               in MkUnitDumpster u'

-- Context to be with the 'YulMonad'.
newtype YulMonadCtx r = MkYulMonadCtx (UnitDumpster r)

instance YulO2 r a => ContextualConsumable (YulMonadCtx r) (P'x eff r a) where
  contextualConsume (MkYulMonadCtx ud) x = MkYulMonadCtx (ud_gulp x ud)

instance YulO2 r a => ContextualDupable (YulMonadCtx r) (P'x eff r a) where
  contextualDup ctx x = (ctx, dup2'l x)


instance YulO1 r => ContextualConsumable (YulMonadCtx r) () where
  contextualConsume (MkYulMonadCtx ud) x = MkYulMonadCtx (lseq x ud)
