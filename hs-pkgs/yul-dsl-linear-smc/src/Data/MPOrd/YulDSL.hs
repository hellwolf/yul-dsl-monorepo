{-# OPTIONS_GHC -Wno-orphans #-}
module Data.MPOrd.YulDSL where

-- linear-base
import Prelude.Linear (Bool (False, True), Consumable, lseq)
-- yul-dsl
import YulDSL.Core
--
import Data.MPOrd

-- | 'MPEq' instance for yul category morphisms.
instance (YulO1 r, YulNumCmp a) => MPEq (YulCat eff r a) (YulCat eff r BOOL) where
  a == b = yulNumEq <.< YulProd a b <.< YulDup
  a /= b = yulNumNe <.< YulProd a b <.< YulDup

-- | 'MPOrd' instance for yul category morphisms.
instance (YulO1 r, YulNumCmp a) => MPOrd (YulCat eff r a) (YulCat eff r BOOL) where
  a  < b = yulNumLt <.< YulProd a b <.< YulDup
  a <= b = yulNumLe <.< YulProd a b <.< YulDup
  a  > b = yulNumGt <.< YulProd a b <.< YulDup
  a >= b = yulNumGe <.< YulProd a b <.< YulDup

-- | Default if-then-else instance for Haskell Bool.
instance Consumable a => IfThenElse Bool a where
  ifThenElse True  a b = lseq b a
  ifThenElse False a b = lseq a b
