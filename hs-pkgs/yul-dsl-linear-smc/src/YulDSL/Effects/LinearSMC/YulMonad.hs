module YulDSL.Effects.LinearSMC.YulMonad
  ( YulMonad, runYulMonad
  -- * Combinators Of Linearly Versioned Monad
  , module Control.LinearlyVersionedMonad.Combinators
  , Control.Functor.Linear.fmap
  ) where
-- linear-base
import Control.Category.Linear                    (discard, ignore)
import Control.Functor.Linear qualified
import Unsafe.Linear                              qualified as UnsafeLinear
-- yul-dsl
import YulDSL.Core
--
import Control.LinearlyVersionedMonad             (LVM, runLVM)
import Control.LinearlyVersionedMonad.Combinators
import Data.LinearContext
--
import YulDSL.Effects.LinearSMC.YulPort


--------------------------------------------------------------------------------
-- YulMonad: A Linearly Versioned Monad for YulDSL
--------------------------------------------------------------------------------

-- | YulMonad is a linear versioned monad with 'YulMonadCtx' as its context data.
type YulMonad va vb r = LVM (YulMonadCtx r) va vb

-- | Run a YulMonad with an initial unit port and returns a versioned result.
runYulMonad :: forall vd r a ue . YulO2 r a
            => P'x ue r () ⊸ YulMonad 0 vd r (P'V vd r a) ⊸ P'V vd r a
runYulMonad u m = let !(ctx', a) = runLVM (MkYulMonadCtx (UnsafeLinear.coerce u)) m
                      !(MkYulMonadCtx (MkUnitDumpster u')) = ctx'
                  in ignore (UnsafeLinear.coerce u') a

--------------------------------------------------------------------------------
-- YulMonad Context
--------------------------------------------------------------------------------

-- A dumpster of units and its utility functions start with "ud_".
newtype UnitDumpster r = MkUnitDumpster (P'V 0 r ())

-- Duplicate a unit.
ud_dupu :: forall eff r. YulO1 r
        => UnitDumpster r ⊸ (UnitDumpster r, P'x eff r ())
ud_dupu (MkUnitDumpster u) = let !(u1, u2) = dup2'l u
                             in (MkUnitDumpster u1, UnsafeLinear.coerce u2)

-- Gulp an input port.
ud_gulp :: forall eff r a. YulO2 r a
        => P'x eff r a ⊸ UnitDumpster r ⊸ UnitDumpster r
ud_gulp x (MkUnitDumpster u) = let u' = ignore u (UnsafeLinear.coerce (discard x))
                               in MkUnitDumpster u'

-- Context to be with the 'YulMonad'.
newtype YulMonadCtx r = MkYulMonadCtx (UnitDumpster r)

instance YulO2 r a => ContextualConsumable (YulMonadCtx r) (P'x eff r a) where
  contextualConsume (MkYulMonadCtx ud) x = MkYulMonadCtx (ud_gulp x ud)

instance YulO2 r a => ContextualDupable (YulMonadCtx r) (P'x eff r a) where
  contextualDup ctx x = (ctx, dup2'l x)

instance YulO2 r a => ContextualEmbeddable (YulMonadCtx r) (P'V v r) a where
  contextualEmbed (MkYulMonadCtx ud) x'p = let !(ud', u') = ud_dupu ud
                                               x'v = emb'l x'p u'
                                           in (MkYulMonadCtx ud', x'v)
