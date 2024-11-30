{-# LANGUAGE DerivingVia #-}
module YulDSL.Effects.LinearSMC.LinearThread
  ( runYulMonad
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


newtype YulMonadCtx = MkYulMonadCtx ()

instance Consumable (YulMonadCtx) where
  consume = UnsafeLinear.coerce -- fixme

type YulMonad s va vb a = LVM s YulMonadCtx va vb a

runYulMonad :: forall r va vb a. (forall s. YulMonad s va vb (P'V vb r a)) ⊸ P'V vb r a
runYulMonad m = let %1 !(ctx', a) = runLVM (MkYulMonadCtx ()) m in lseq ctx' a

-- | Safe operation that lifts a pure yul port to a versioned yul port.
lift'l :: forall v s r a. YulO2 r a
       => P'P r a ⊸ YulMonad s v v (P'V v r a)
lift'l x = MkLVM \(lt, ctx) -> lseq lt (Dict, ctx, UnsafeLinear.coerce x)

fin'with :: forall v s r a. YulO2 r a
         => P'V v r a ⊸ YulMonad s v v (P'V v r a)
fin'with x = MkLVM \(lt, ctx) -> lseq lt (Dict, ctx, x)

fin'dis :: forall v s r a. YulO2 r a
        => P'V v r a ⊸ YulMonad s v v (P'V v r ())
fin'dis x = MkLVM \(lt, ctx) -> lseq lt (Dict, ctx, discard x)

fin'emb :: forall v s r a b. YulO3 r a b
        => a -> (P'V v r b ⊸ YulMonad s v v (P'V v r a))
fin'emb a b = MkLVM \(lt, ctx) -> lseq lt (Dict, ctx, (emb'l a b))

fin'const :: forall v s r a b. YulO3 r a b
          => P'V v r a -> (P'V v r b ⊸ YulMonad s v v (P'V v r a))
fin'const a b = MkLVM \(lt, ctx) -> lseq lt (Dict, ctx, const'l a b)

sget :: forall v s r a. (YulO2 r a, ABIWordValue a)
     => P'V v r ADDR ⊸ YulMonad s v v (P'V v r a)
sget a = MkLVM \(lt, ctx) -> lseq lt (Dict, ctx, encode YulSGet a)

sput :: forall v s r a. (YulO2 r a, ABIWordValue a)
     => P'V v r ADDR ⊸ P'V v r a ⊸ YulMonad s v (v + 1) (P'V (v + 1) r a)
sput to x =
  dup2'l x
  & \(x1, x2) -> encode YulSPut (merge (to, x1))
  & \u -> MkLVM \(lt, ctx) -> lseq lt (unsafeAxiom, ctx, UnsafeLinear.coerce (ignore u x2))

sput_ :: forall v s r a. (YulO2 r a, ABIWordValue a)
      => P'V v r ADDR ⊸ P'V v r a ⊸ YulMonad s v (v + 1) (P'V (v + 1) r ())
sput_ to x =
  encode YulSPut (merge (to, x))
  & \u -> MkLVM \(lt, ctx) -> lseq lt (unsafeAxiom, ctx, UnsafeLinear.coerce u)

sputAt :: forall v s r a. (YulO2 r a, ABIWordValue a)
       => ADDR ⊸ P'V v r a ⊸ YulMonad s v (v + 1) (P'V (v + 1) r a)
sputAt to x = mkUnit x & \(x', u) -> emb'l to u & \a -> sput a x'
