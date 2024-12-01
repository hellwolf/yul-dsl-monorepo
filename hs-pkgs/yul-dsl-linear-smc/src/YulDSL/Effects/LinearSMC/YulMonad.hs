{-# LANGUAGE DerivingVia #-}
module YulDSL.Effects.LinearSMC.YulMonad
  ( YulMonad, runYulMonad
  , lift'l
  , fin'with, fin'dis, fin'emb, fin'const
  , sget, sput, sput_, sputAt
  ) where

-- base
import           GHC.TypeLits                     (type (+))
-- constraints
import           Data.Constraint                  (Dict (Dict))
import           Data.Constraint.Unsafe           (unsafeAxiom)
-- linear-base
import           Control.Category.Linear
import           Prelude.Linear
import qualified Unsafe.Linear                    as UnsafeLinear
-- yul-dsl
import           YulDSL.Core
--
import           Control.LinearVersionedMonad
import           YulDSL.Effects.LinearSMC.YulPort

-- | YulMonad is a linear versioned monad with 'YulMonadCtx' as its context data.
type YulMonad va vb r = LVM (YulMonadCtx r) va vb

runYulMonad :: forall vd r a ue . YulO2 r a
            => P'x ue r () ⊸ YulMonad 0 vd r (P'V vd r a) ⊸ P'V vd r a
runYulMonad u m = let !(ctx', a) = runLVM (MkYulMonadCtx (UnsafeLinear.coerce u)) m
                      !(MkYulMonadCtx (MkUnitDumpster u')) = ctx'
                  in ignore (UnsafeLinear.coerce u') a

-- | Safe operation that lifts a pure yul port to a versioned yul port.
lift'l :: forall v r a. YulO2 r a
       => P'P r a ⊸ YulMonad v v r (P'V v r a)
lift'l x = MkLVM \(lt, ctx) -> lseq lt (Dict, ctx, UnsafeLinear.coerce x)

fin'with :: forall v r a. YulO2 r a
         => P'V v r a ⊸ YulMonad v v r (P'V v r a)
fin'with x = MkLVM \(lt, ctx) -> lseq lt (Dict, ctx, x)

fin'dis :: forall v r a. YulO2 r a
        => P'V v r a ⊸ YulMonad v v r (P'V v r ())
fin'dis x = MkLVM \(lt, ctx) -> lseq lt (Dict, ctx, discard x)

fin'emb :: forall v r a b. YulO3 r a b
        => a -> (P'V v r b ⊸ YulMonad v v r (P'V v r a))
fin'emb a b = MkLVM \(lt, ctx) -> lseq lt (Dict, ctx, (emb'l a b))

fin'const :: forall v r a b. YulO3 r a b
          => P'V v r a -> (P'V v r b ⊸ YulMonad v v r (P'V v r a))
fin'const a b = MkLVM \(lt, ctx) -> lseq lt (Dict, ctx, const'l a b)

sget :: forall v r a. (YulO2 r a, ABIWordValue a)
     => P'V v r ADDR ⊸ YulMonad v v r (P'V v r a)
sget a = MkLVM \(lt, ctx) -> lseq lt (Dict, ctx, encode YulSGet a)

sput :: forall v r a. (YulO2 r a, ABIWordValue a)
     => P'V v r ADDR ⊸ P'V v r a ⊸ YulMonad v (v + 1) r (P'V (v + 1) r a)
sput to x =
  dup2'l x
  & \(x1, x2) -> encode YulSPut (merge (to, x1))
  & \u -> MkLVM \(lt, ctx) -> lseq lt (unsafeAxiom, ctx, UnsafeLinear.coerce (ignore u x2))

sput_ :: forall v r a. (YulO2 r a, ABIWordValue a)
      => P'V v r ADDR ⊸ P'V v r a ⊸ YulMonad v (v + 1) r (P'V (v + 1) r ())
sput_ to x =
  encode YulSPut (merge (to, x))
  & \u -> MkLVM \(lt, ctx) -> lseq lt (unsafeAxiom, ctx, UnsafeLinear.coerce u)

sputAt :: forall v r a. (YulO2 r a, ABIWordValue a)
       => ADDR ⊸ P'V v r a ⊸ YulMonad v (v + 1) r (P'V (v + 1) r a)
sputAt to x = mkUnit x & \(x', u) -> emb'l to u & \a -> sput a x'


--
-- Internal Stuff
--

-- | A dumpster of unitals.
newtype UnitDumpster r = MkUnitDumpster (P'V 0 r ())

gulp_unit :: forall eff r a. YulO2 r a => P'x eff r a ⊸ UnitDumpster r ⊸ UnitDumpster r
gulp_unit x (MkUnitDumpster u) = let u' = ignore (UnsafeLinear.coerce (discard x)) u
                                 in MkUnitDumpster u'

-- Context to be with the 'YulMonad'.
newtype YulMonadCtx r = MkYulMonadCtx (UnitDumpster r)

instance YulO2 r a => ContextualConsumable (YulMonadCtx r) (P'x eff r a) where
  contextualConsume (MkYulMonadCtx ud) x = MkYulMonadCtx (gulp_unit x ud)
