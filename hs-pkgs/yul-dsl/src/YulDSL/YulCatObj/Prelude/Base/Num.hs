{-# OPTIONS_GHC -Wno-orphans #-}
{-|

Copyright   : (c) 2023-2024 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : hellwolf@yolc.dev
Stability   : experimental

= Description

This module defines the 'Num' instance from base library for 'YulNum' objects.

-}
module YulDSL.YulCatObj.Prelude.Base.Num where

--
import YulDSL.Core.YulCat
import YulDSL.Core.YulCatObj
import YulDSL.Core.YulNum

instance (YulO2 a r, YulNum a) => Num (YulCat eff r a) where
  a + b = YulJmpB (yulNumAdd @a) <.< YulProd a b <.< YulDup
  a - b = YulJmpB (yulNumSub @a) <.< YulProd a b <.< YulDup
  a * b = YulJmpB (yulNumMul @a)  <.< YulProd a b <.< YulDup
  abs = YulComp (YulJmpB (yulNumAbs @a))
  signum = YulComp (YulJmpB (yulNumSig @a))
  fromInteger = YulEmb . fromInteger
