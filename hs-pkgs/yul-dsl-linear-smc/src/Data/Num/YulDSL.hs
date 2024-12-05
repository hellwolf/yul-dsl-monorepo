{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Num.YulDSL where

-- base
import qualified Prelude        as Prelude.Base
-- linear-base
import           Prelude.Linear
import qualified Unsafe.Linear  as UnsafeLinear
-- yul-dsl
import           YulDSL.Core


--
-- FromInteger instances for ABI types
--

instance FromInteger Integer where
  fromInteger = id

instance (KnownBool s, ValidINTn n) => FromInteger (INTx s n) where
  fromInteger = UnsafeLinear.toLinear Prelude.Base.fromInteger

instance (KnownBool s, ValidINTn n) => FromInteger (Maybe (INTx s n)) where
  fromInteger = UnsafeLinear.toLinear Prelude.Base.fromInteger

--
--  Num instances for (YulCat r a)
--

instance (YulObj r, YulNum a) => Additive (YulCat eff r a) where
  a + b = YulNumAdd <.< YulProd a b <.< YulDup

instance (YulObj r, YulNum a) => AddIdentity (YulCat eff r a) where
  zero = YulEmbed (fromIntegral (0 :: Integer))

instance (YulObj r, YulNum a) => AdditiveGroup (YulCat eff r a) where
  negate a = YulNumNeg <.< a
