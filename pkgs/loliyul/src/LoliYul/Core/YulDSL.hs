{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

{-|

Copyright   : (c) Miao ZhiCheng, 2023
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental


= Description

[Yul](https://docs.soliditylang.org/en/latest/yul.html) is an intermediate language that is part of the [solidity
compiler](https://docs.soliditylang.org/en/latest/). It is by-design aspiring to be compiled to bytecode for
different backends, while at the moment it is for [Ethereum Virtual
Machine](https://ethereum.org/en/developers/docs/evm/) (EVM).

This module provides an "Embedded (in Haskell) Domain Specific Language" (EDSL) for programming in Yul, called
'YulDSL'. Further more, the 'YulDSL' is instantiated as a "Symmetric Monoidal Category" (SMC). Being a SMC enables
the possibility for compiling linearly-typed functions in Haskell directly to the 'YulDSL', that is believed to
greatly enhance the ergonomics of programming in 'YulDSL'.

-}

module LoliYul.Core.YulDSL
  ( YulDSL (..)
  , Fn
  , module LoliYul.Core.YulDSL.Obj
  , module LoliYul.Core.YulDSL.Coerce
  ) where

import           Control.Category.Constrained (Cartesian (dis, dup), Category (..), Monoidal (..), type (⊗))

import           LoliYul.Core.ContractABI
import           LoliYul.Core.YulDSL.Coerce
import           LoliYul.Core.YulDSL.Obj


-- | A GADT-style DSL for Yul that constructs morphisms between its objects.
data YulDSL a b where
  YulCoerce :: forall a b. (YulO2 a b, YulCoercible a b) => YulDSL a b
  -- SMC Primitives
  YulId   :: forall a.       YulO2 a a     => YulDSL a a
  YulComp :: forall a b c.   YulO3 a b c   => YulDSL c b -> YulDSL a c -> YulDSL a b
  YulProd :: forall a b c d. YulO4 a b c d => YulDSL a b -> YulDSL c d -> YulDSL (a⊗c) (b⊗d)
  YulSwap :: forall a b.     YulO2 a b     => YulDSL (a⊗b) (b⊗a)
  YulDis  :: forall a.       YulO1 a       => YulDSL a ()
  YulDup  :: forall a.       YulO1 a       => YulDSL a (a⊗a)
  -- Yul Primitives
  YulConst    :: forall a b. YulO2 a b => b -> YulDSL a b
  YulNumNeg   :: forall a.   YulNum a  => YulDSL a a
  YulNumAdd   :: forall a.   YulNum a  => YulDSL (a⊗a) a
  YulSGet     :: forall a.   YulVal a  => YulDSL ADDR a
  YulSPut     :: forall a.   YulVal a  => YulDSL (ADDR⊗a) ()
  YulInternFn :: forall a b. YulO2 a b => String -> YulDSL a b -> Fn a b
  -- YulAbiEnc   :: YulObj a => YulDSL a BYTES
  -- YulAbiDec   :: YulObj a => YulDSL BYTES (Maybe a)
  -- YulExternFn :: YulO2 a b => T.Text -> YulDSL a b -> YulErrorHandler -> FnExt
  -- YulJmpCall
  -- YulExtCall

type Fn a b = YulDSL a b
-- type FnExt = YulDSL BYTES BYTES
-- type YulErrorHandler = YulDSL BYTES ()

instance Category YulDSL where
  type Obj YulDSL = YulObj
  id = YulId
  (∘) = YulComp

instance Monoidal YulDSL where
  (×)     = YulProd
  unitor  = YulCoerce
  unitor' = YulCoerce
  assoc   = YulCoerce
  assoc'  = YulCoerce
  swap    = YulSwap

instance Cartesian YulDSL where
  dis = YulDis
  dup = YulDup

deriving instance Show (YulDSL a b)
