{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Num.YulDSL.LinearSMC where

-- base
import qualified Prelude                          as Prelude.Base
-- linear-base
import           Prelude.Linear
import qualified Unsafe.Linear                    as UnsafeLinear
-- linear-smc
import           Control.Category.Linear          (encode, merge)
-- yul-dsl
import           YulDSL.Core
--
import           YulDSL.Effects.LinearSMC.YulPort


{- * FromInteger instances -}

instance FromInteger Integer where
  fromInteger = id

instance (KnownBool s, ValidINTn n) => FromInteger (INTx s n) where
  fromInteger = UnsafeLinear.toLinear Prelude.Base.fromInteger

instance (KnownBool s, ValidINTn n) => FromInteger (Maybe (INTx s n)) where
  fromInteger = UnsafeLinear.toLinear Prelude.Base.fromInteger

{- * Num instances for (YulCat r a) -}

instance (YulObj r, YulNum a) => Additive (YulCat eff r a) where
  a + b = YulNumAdd <.< YulProd a b <.< YulDup

instance (YulObj r, YulNum a) => AddIdentity (YulCat eff r a) where
  zero = YulEmbed (fromIntegral (0 :: Integer))

instance (YulObj r, YulNum a) => AdditiveGroup (YulCat eff r a) where
  negate a = YulNumNeg <.< a

{- * Num instances for (P'V v r) -}

instance (YulNum a, YulObj r) => Additive (P'V v r a) where
  a + b = encode YulNumAdd (merge (a, b))

instance (YulNum a, YulObj r) => AddIdentity (P'V v r a) where
  -- Note: uni-port is forbidden in linear-smc, but linear-base AdditiveGroup requires this instance.
  zero = error "unit is undefined for linear ports"

instance (YulNum a, YulObj r) => AdditiveGroup (P'V v r a) where
  negate = encode YulNumNeg
