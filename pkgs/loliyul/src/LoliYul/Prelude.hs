{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

This module packages all the goodies prelude-worthy for programming "YulDSL" in linear-types.

-}

module LoliYul.Prelude
( module Prelude.Linear
, module Control.Category.Linear
, module LoliYul.Linear
) where

-- linear-base
import           Prelude.Linear
-- linear-smc
import           Control.Category.Linear
-- loliyul
import           LoliYul.Linear
