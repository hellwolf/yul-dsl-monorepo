{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module LoliYul.Core.YulCat where

import           Data.Constraint              (Dict (..))
import qualified Data.Text                    as T

import           Control.Category.Constrained (Cartesian (dis, dup),
                                               Category (..), Monoidal (..),
                                               ProdObj (..), type (⊗))

import           LoliYul.Core.Types


--------------------------------------------------------------------------------
-- YulCat as a DSL for Yul
--------------------------------------------------------------------------------

-- | A GADT-style DSL for Yul
data YulCat a b where
  -- SMC Primitives
  YulIdentity :: YulCat a a
  YulCompose  :: YulCat c b -> YulCat a c -> YulCat a b
  YulProduct  :: YulCat c d -> YulCat p q -> YulCat (c⊗p) (d⊗q)
  YulUnitorL  :: YulCat a (a⊗())
  YulUnitorR  :: YulCat (a⊗()) a
  YulAssocL   :: YulCat ((a⊗b)⊗c) (a⊗(b⊗c))
  YulAssocR   :: YulCat (a⊗(b⊗c)) ((a⊗b)⊗c)
  YulSwap     :: YulCat (a⊗b) (b⊗a)
  YulDis      :: YulCat a ()
  YulDup      :: YulCat a (a⊗a)
  -- Yul Primitives
  YulConst    :: YulCon b => b -> YulCat a b
  YulSGet     :: YulVal a => YulCat YulAddr a
  YulSPut     :: YulVal a => YulCat (YulAddr⊗a) ()
  YulNumPlus  :: YulNum a => YulCat (a⊗a) a
  YulNumMinus :: YulNum a => YulCat (a⊗a) a
  YulDetuple  :: YulVal a => YulCat [YulType] (Maybe a⊗[YulType])
  YulJust     :: YulCat (Maybe a) a
  YulFunc     :: T.Text -> YulCat a b -> YulFunction

data YulTypeSpec = YulTypeSpec deriving Show

type YulFunction = YulCat [YulType] [YulType]

--------------------------------------------------------------------------------
-- YulCat as a Symmetric Monoidal Category for Yul
--------------------------------------------------------------------------------

-- | Objects in YulCat are instances of YulCon.
class Show a => YulCon a where
  subYulCon :: forall c b. (YulCon (c ⊗ b), a ~ (c ⊗ b)) => Dict (YulCon c, YulCon b)
  subYulCon = error "YulCon::subYulCon only defined for objprod"

class YulCon a => YulVal a

class YulVal a => YulNum a

instance YulCon ()

instance YulCon YulBool
instance YulVal YulBool

instance YulCon YulAddr
instance YulVal YulAddr
instance YulNum YulAddr -- TODO remove

instance YulCon YulUInt
instance YulVal YulUInt
instance YulNum YulUInt

instance YulCon YulInt
instance YulVal YulInt
instance YulNum YulInt

instance YulCon [YulType]

instance YulCon YulType

instance YulCon a => YulCon (Maybe a)

instance (YulCon a, YulCon b) => YulCon (a⊗b) where
  subYulCon = Dict

instance ProdObj YulCon where
  prodobj = Dict
  objprod = subYulCon
  objunit = Dict

instance Category YulCat where
  type Obj YulCat = YulCon
  id = YulIdentity
  (∘) = YulCompose

instance Monoidal YulCat where
  (×)     = YulProduct
  unitor  = YulUnitorL
  unitor' = YulUnitorR
  assoc   = YulAssocL
  assoc'  = YulAssocR
  swap    = YulSwap

instance Cartesian YulCat where
  dis = YulDis
  dup = YulDup

deriving instance Show (YulCat a b)
