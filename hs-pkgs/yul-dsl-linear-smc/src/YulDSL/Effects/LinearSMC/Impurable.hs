{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
module YulDSL.Effects.LinearSMC.Impurable
  ( Impurable (impure), impureN
  ) where
-- yul-dsl
import YulDSL.Core
-- linear-base
import Unsafe.Linear                     qualified as UnsafeLinear
-- lvm
import Control.LinearlyVersionedMonad    qualified as LVM
--
import YulDSL.Effects.LinearSMC.YulMonad
import YulDSL.Effects.LinearSMC.YulPort


class Impurable xs'p xs'v v r | xs'v -> xs'p where
  -- | Safe operation that impures a pure yul port to a versioned yul port.
  impure :: forall. xs'p ⊸ YulMonad v v r xs'v

instance forall a v r. Impurable (P'P r a) (P'V v r a) v r where
  impure x = pure (UnsafeLinear.coerce x)

instance forall v r. Impurable (NP '[]) (NP '[]) v r where
  impure Nil = pure Nil

instance forall x'p x'v xs'p xs'v v r.
         ( Impurable x'p x'v v r
         , Impurable (NP xs'p) (NP xs'v) v r
         ) => Impurable (NP (x'p:xs'p)) (NP (x'v:xs'v)) v r where
  impure (x :* xs) = LVM.do
    x' <- impure x
    xs' <- impure xs
    LVM.pure (x' :* xs')

impureN :: forall v r tpl'p tpl'v.
           ( ConvertibleTupleN tpl'p
           , ConvertibleTupleN tpl'v
           -- , TupleNtoNP tpl'p ~ NP (P'P r a:xs)
           -- , TupleNtoNP tpl'v ~ NP (P'V v r a:xs)
           , Impurable (TupleNtoNP tpl'p) (TupleNtoNP tpl'v) v r
           )
        => tpl'p ⊸ YulMonad v v r tpl'v
impureN tpl'p = LVM.do
  np'v :: TupleNtoNP tpl'v <- impure (fromTupleNtoNP tpl'p)
  LVM.pure (fromNPtoTupleN np'v)
