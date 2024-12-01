{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : hellwolf@yolc.dev
Stability   : experimental

= Description

This module packages all the goodies prelude-worthy for programming "YulDSL" in linear-types.

-}

module Prelude.YulDSL
  ( -- linear-base
    module Prelude.Linear
    -- linear-smc
  , module Control.Category.Linear
    -- yul-dsl
  , module YulDSL.Core
    --
  , module Data.MPOrd
  , module YulDSL.Effects.LinearSMC
  ) where

-- linear-base
import           Prelude.Linear            hiding (Eq (..), Ord (..))
-- linear-smc
import           Control.Category.Linear
-- yul-dsl
import           YulDSL.Core
--
import           Data.MPOrd
import           Data.MPOrd.YulDSL         ()
import           Data.Num.YulDSL.LinearSMC ()
import           YulDSL.Effects.LinearSMC
