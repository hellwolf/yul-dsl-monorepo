{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Num.YulDSL.Linear where

-- base
import qualified Prelude                             as Prelude.Base
-- linear-base
import           Prelude.Linear
import qualified Unsafe.Linear                       as UnsafeLinear
-- linear-smc
import           Control.Category.Linear             (encode, merge)
-- yul-dsl
import           YulDSL.Core
--
import           Ethereum.ContractsABI.YulDSL.Linear


{- * FromInteger instances -}

instance FromInteger Integer where
  fromInteger = id

instance (KnownBool s, ValidINTn n) => FromInteger (INTx s n) where
  fromInteger = UnsafeLinear.toLinear Prelude.Base.fromInteger

instance (KnownBool s, ValidINTn n) => FromInteger (Maybe (INTx s n)) where
  fromInteger = UnsafeLinear.toLinear Prelude.Base.fromInteger

{- * Num instances for (YulCat r a) -}

instance (YulObj r, YulNum a) => Additive (YulCat r a) where
  a + b = YulNumAdd <.< YulProd a b <.< YulDup

instance (YulObj r, YulNum a) => AddIdentity (YulCat r a) where
  zero = YulEmbed (fromIntegral (0 :: Integer))

instance (YulObj r, YulNum a) => AdditiveGroup (YulCat r a) where
  negate a = YulNumNeg <.< a

{- * Num instances for (Yul'P  r) -}

instance {-# OVERLAPPABLE #-} (YulNum a, YulObj r) => Additive (Yul'P r a) where
  a + b = encode YulNumAdd (merge (a, b))

instance {-# OVERLAPPABLE #-} (YulNum a, YulObj r) => AddIdentity (Yul'P r a) where
  -- Note: uni-port is forbidden in linear-smc, but linear-base AdditiveGroup requires this instance.
  zero = error "unit is undefined for linear ports"

instance {-# OVERLAPPABLE #-} (YulNum a, YulObj r) => AdditiveGroup (Yul'P r a) where
  negate = encode YulNumNeg

----------------------------------------------------------------------

{- * MP Ord -}

instance (YulObj r, YulNum a) => MPOrd (Yul'P r a) (BOOL'P r) where
  a  <? b = encode (YulNumCmp (true , false, false)) (merge (a, b))
  a <=? b = encode (YulNumCmp (true , true , false)) (merge (a, b))
  a  >? b = encode (YulNumCmp (false, false, true )) (merge (a, b))
  a >=? b = encode (YulNumCmp (false, true , true )) (merge (a, b))
  a ==? b = encode (YulNumCmp (false, true , false)) (merge (a, b))
  a /=? b = encode (YulNumCmp (true , false, true )) (merge (a, b))

{- * Control flow utilities -}

instance YulO2 a r => IfThenElse (BOOL'P r) (Yul'P r a) where
  ifThenElse c a b = encode YulITE (merge(c, merge(a, b)))

instance Consumable a => IfThenElse Bool a where
  ifThenElse True  a b = lseq b a
  ifThenElse False a b = lseq a b
