{-# LANGUAGE ExplicitNamespaces #-}
{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

The Core of the YulDSL is the 'YulDSL.Core.ContractABI' & 'YulDSL.Core.YulDSL' modules.

  - To evaluate the 'YulDSL' in simulation, one should use the "YulDSL.Eval" module.

  - To program in linear-types, one should benefit from using combinators in "YulDSL.Linear", and replace with
    linear-types friendly "YulDSL.Prelude".

  - To generate code, one should fine a suitable generator under the "YulDSL.CodeGen" namespace.
-}

module YulDSL.Core
  ( module Ethereum.ContractABI
  , module YulDSL.Core.YulCat
  , module YulDSL.Core.YulObject
  ) where

import           Ethereum.ContractABI
import           YulDSL.Core.YulCat
import           YulDSL.Core.YulObject
