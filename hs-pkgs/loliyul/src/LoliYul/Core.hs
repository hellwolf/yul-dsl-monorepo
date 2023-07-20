{-# LANGUAGE ExplicitNamespaces #-}
{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

The Core of the LoliYul is the 'LoliYul.Core.ContractABI' & 'LoliYul.Core.YulDSL' modules.

  - To evaluate the 'YulDSL' in simulation, one should use the "LoliYul.Eval" module.

  - To program in linear-types, one should benefit from using combinators in "LoliYul.Linear", and replace with
    linear-types friendly "LoliYul.Prelude".

  - To generate code, one should fine a suitable generator under the "LoliYul.CodeGen" namespace.
-}

module LoliYul.Core
  ( module LoliYul.Core.ContractABI
  , module LoliYul.Core.YulDSL
  , type (⊗)
  ) where

import           LoliYul.Core.ContractABI
import           LoliYul.Core.YulDSL

import           Control.Category.Constrained (type (⊗))
