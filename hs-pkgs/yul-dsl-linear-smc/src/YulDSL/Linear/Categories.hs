{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

Categories required for being a symmetric monoidal category.

-}

module YulDSL.Linear.Categories () where

-- constraints
import           Data.Constraint              (Dict (..))
-- linear-smc
import           Control.Category.Constrained (Cartesian (dis, dup), Category (..), Monoidal (..), ProdObj (..))
--
import           YulDSL.Core.ContractABI
import           YulDSL.Core.YulCat           (YulCat (..), YulObj)

-- | Instance for linear-smc 'ProdObj' for the objects in the category.
instance ProdObj YulObj where
  prodobj = Dict
  objprod = maybe_prod_objs
  objunit = Dict

instance Category YulCat where
  type Obj YulCat = YulObj
  id  = YulId
  (∘) = YulComp

instance Monoidal YulCat where
  (×)     = YulProd
  unitor  = YulCoerce
  unitor' = YulCoerce
  assoc   = YulCoerce
  assoc'  = YulCoerce
  swap    = YulSwap

instance Cartesian YulCat where
  dis = YulDis
  dup = YulDup
