{-# LANGUAGE FunctionalDependencies #-}
module YulDSL.Effects.LinearSMC.Impurable
  ( Impurable (impure'l), impure, impureN
  ) where
-- base
import Data.Type.Equality                (type (~))
-- yul-dsl
import YulDSL.Core
-- linear-base
import Unsafe.Linear                     qualified as UnsafeLinear
-- lvm
import Control.LinearlyVersionedMonad    qualified as LVM
--
import YulDSL.Effects.LinearSMC.YulMonad
import YulDSL.Effects.LinearSMC.YulPort


class Impurable x'p x'v | x'v -> x'p where
  -- | Safe operation that impures a pure yul port to a versioned yul port.
  impure'l :: forall. x'p ⊸ x'v

instance forall a v r. Impurable (P'P r a) (P'V v r a) where
  impure'l = UnsafeLinear.coerce

class ImpurableNP np'p np'v where
  impure_np :: forall. np'p ⊸ np'v

instance forall. ImpurableNP (NP '[]) (NP '[]) where
  impure_np Nil = Nil

impure :: forall x'p x'v r v. (Impurable x'p x'v) => x'p ⊸ YulMonad v v r x'v
impure x = pure (impure'l x)

instance forall x'p x'v xs'p xs'v.
         ( Impurable x'p x'v
         , ImpurableNP (NP xs'p) (NP xs'v)
         ) => ImpurableNP (NP (x'p:xs'p)) (NP (x'v:xs'v)) where
  impure_np (x :* xs) = impure'l x :* impure_np xs

impureN :: forall tpl'p tpl'v x xs v r.
           ( ConvertibleTupleN tpl'p
           , ConvertibleTupleN tpl'v
           -- , TupleNtoNP tpl'p ~ NP (P'P r x:xs) -- WARNING: DO NOT OPEN THIS, FUNDEP HELL WILL BREAK LOOSE
           , TupleNtoNP tpl'v ~ NP (P'V v r x:xs)
           , ImpurableNP (TupleNtoNP tpl'p) (TupleNtoNP tpl'v)
           )
        => tpl'p ⊸ YulMonad v v r tpl'v
impureN tpl'p = let !(np'v :: TupleNtoNP tpl'v) = impure_np (fromTupleNtoNP tpl'p)
                in LVM.pure (fromNPtoTupleN np'v)
