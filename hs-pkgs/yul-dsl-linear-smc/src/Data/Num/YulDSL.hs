{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Num.YulDSL where

-- base
import qualified Prelude        as Prelude.Base
-- linear-base
import           Prelude.Linear
import qualified Unsafe.Linear  as UnsafeLinear
-- yul-dsl
import           YulDSL.Core


instance FromInteger Integer where
  fromInteger = id

instance (KnownBool s, KnownNat n) => FromInteger (INTx s n) where
  fromInteger = UnsafeLinear.toLinear Prelude.Base.fromInteger

instance (KnownBool s, KnownNat n) => FromInteger (Maybe (INTx s n)) where
  fromInteger = UnsafeLinear.toLinear Prelude.Base.fromInteger

{- * Maybe YulNum -}

instance (YulObj r, YulNum a) => Additive (YulCat r (Maybe a)) where
  a + b = YulNumAdd <.< YulProd a b <.< YulDup

instance (YulObj r, YulNum a) => AddIdentity (YulCat r (Maybe a)) where
  zero = YulEmbed (fromIntegral (0 :: Integer))

instance (YulObj r, YulNum a) => AdditiveGroup (YulCat r (Maybe a)) where
  negate a = YulNumNeg <.< a

{- * YulNum -}
