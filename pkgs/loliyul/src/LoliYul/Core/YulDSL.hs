{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module LoliYul.Core.YulDSL where

import           Data.Constraint              (Dict (..))
import qualified Data.Text                    as T
import           Data.Typeable

import           Control.Category.Constrained (Cartesian (dis, dup),
                                               Category (..), Monoidal (..),
                                               ProdObj (..), type (⊗))

import           LoliYul.Core.Types


--------------------------------------------------------------------------------
-- YulDSL, a DSL for Yul.
--------------------------------------------------------------------------------

-- | YulDSL objects.
class (Show a, Typeable a) => YulObj a where
  prod_objs :: forall b c. a ~ (b ⊗ c) => Dict (YulObj b, YulObj c)
  prod_objs = error "YulObj::prod_objs is only defined for a⊗b"

type YulO2 a b = (YulObj a, YulObj b)
type YulO3 a b c = (YulObj a, YulObj b, YulObj c)
type YulO4 a b c d = (YulObj a, YulObj b, YulObj c, YulObj d)
type YulO5 a b c d e = (YulObj a, YulObj b, YulObj c, YulObj d, YulObj e)

instance YulObj ()
instance YulObj AbiBool
instance YulObj AbiAddr
instance YulObj AbiUInt
instance YulObj AbiInt
instance YulObj AbiBytes

instance YulObj a => YulObj (Maybe a)

instance (YulObj a, YulObj b) => YulObj (a⊗b) where
  prod_objs = Dict

instance (YulObj a, YulObj b) => YulObj (a :> b)

-- | Yul value objects.
class YulObj a => YulVal a

instance YulVal AbiBool
instance YulVal AbiAddr
instance YulVal AbiUInt
instance YulVal AbiInt

-- | Yul number-value objects.
class YulVal a => YulNum a
instance YulNum AbiAddr -- FIXME remove
instance YulNum AbiUInt
instance YulNum AbiInt

-- | Family of objects that have the same bytes representations.
class YulO2 a b => YulSameBytes a b
instance YulObj a => YulSameBytes a a
instance YulObj a => YulSameBytes (a⊗()) a
instance YulObj a => YulSameBytes a (a⊗())
instance YulO2 a as => YulSameBytes (a :> as) (a⊗as)
instance YulO3 a b c => YulSameBytes (a⊗(b⊗c)) ((a⊗b)⊗c)
instance YulO3 a b c => YulSameBytes ((a⊗b)⊗c) (a⊗(b⊗c))

-- | A GADT-style DSL for Yul that constructs morphisms between its objects.
data YulDSL a b where
  -- SMC Primitives
  YulId   :: forall a b. (YulO2 a b, YulSameBytes a b) => YulDSL a b
  YulComp :: YulO3 a b c => YulDSL c b -> YulDSL a c -> YulDSL a b
  YulProd :: YulO4 a b c d => YulDSL a b -> YulDSL c d -> YulDSL (a⊗c) (b⊗d)
  YulSwap :: YulO2 a b => YulDSL (a⊗b) (b⊗a)
  YulDis  :: YulObj a => YulDSL a ()
  YulDup  :: YulObj a => YulDSL a (a⊗a)
  -- Yul Primitives
  YulConst    :: YulO2 a b => b -> YulDSL a b
  YulNumNeg   :: YulNum a => YulDSL a a
  YulNumAdd   :: YulNum a => YulDSL (a⊗a) a
  YulSGet     :: YulVal a => YulDSL AbiAddr a
  YulSPut     :: YulVal a => YulDSL (AbiAddr⊗a) ()
  -- YulAbiEnc   :: YulObj a => YulDSL a AbiBytes
  -- YulAbiDec   :: YulObj a => YulDSL AbiBytes (Maybe a)
  YulInternFn :: YulO2 a b => T.Text -> YulDSL a b -> YulInternalFunction a b
  -- YulExternFn :: YulO2 a b => T.Text -> YulDSL a b -> YulErrorHandler -> YulExternalFunction
  -- YulJmpCall
  -- YulExtCall
  deriving Typeable

type YulInternalFunction a b = YulDSL a b
type YulErrorHandler = YulDSL AbiBytes ()
type YulExternalFunction = YulDSL AbiBytes AbiBytes

--------------------------------------------------------------------------------
-- YulDSL as a Symmetric Monoidal Category for Yul
--------------------------------------------------------------------------------

instance ProdObj YulObj where
  prodobj = Dict
  objprod = prod_objs
  objunit = Dict

instance Category YulDSL where
  type Obj YulDSL = YulObj
  id = YulId
  (∘) = YulComp

instance Monoidal YulDSL where
  (×)     = YulProd
  unitor  = YulId
  unitor' = YulId
  assoc   = YulId
  assoc'  = YulId
  swap    = YulSwap

instance Cartesian YulDSL where
  dis = YulDis
  dup = YulDup

deriving instance Show (YulDSL a b)
