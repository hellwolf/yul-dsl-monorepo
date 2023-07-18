{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

{-|

Copyright   : (c) 2023 Miao, ZhiCheng
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
  , Fn (..)
  , module LoliYul.Core.YulDSL.Obj
  , module LoliYul.Core.YulDSL.Coerce
  ) where

import           Control.Category.Constrained (Cartesian (dis, dup), Category (..), Monoidal (..), type (⊗))

import           LoliYul.Core.ContractABI
import           LoliYul.Core.YulDSL.Coerce
import           LoliYul.Core.YulDSL.Obj


-- | A GADT-style DSL for Yul that constructs morphisms between its objects.
data YulDSL a b where
  -- SMC Primitives
  --
  YulCoerce :: forall a b. (YulO2 a b, YulCoercible a b) => YulDSL a b
  --
  YulId     :: forall a.       YulO2 a a     => YulDSL a a
  YulComp   :: forall a b c.   YulO3 a b c   => YulDSL c b -> YulDSL a c -> YulDSL a b
  YulProd   :: forall a b c d. YulO4 a b c d => YulDSL a b -> YulDSL c d -> YulDSL (a ⊗ c) (b ⊗ d)
  YulSwap   :: forall a b.     YulO2 a b     => YulDSL (a ⊗ b) (b ⊗ a)
  YulDis    :: forall a.       YulO1 a       => YulDSL a ()
  YulDup    :: forall a.       YulO1 a       => YulDSL a (a ⊗ a)

  -- Control Flow Primitives
  --
  -- | Embed a constant value, and ignore the input.
  YulConst :: forall a b. YulO2 a b => b -> YulDSL a b
  -- | Apply the function object over an argument.
  YulApfun :: forall a b. YulO2 a b => String -> YulDSL a b
  -- | Mapping over a list.
  YulMap   :: forall a b. YulO2 a b => YulDSL a b -> YulDSL [a] [b]
  -- | Folding over a list from the left.
  YulFoldl :: forall a b. YulO2 a b => YulDSL (b ⊗ a) b -> YulDSL [a] b
  -- | EVM Call.
  YulCall  :: forall a b. YulO2 a b => YulDSL ((CallSpec a b) ⊗ a) (Maybe b)
  -- | If-then-else.
  YulITE   :: forall a  . YulO1 a   => YulDSL (BOOL ⊗ (a ⊗ a)) a

  -- YulVal Primitives
  --
  -- - Boolean Operatiosn
  YulNOT    :: YulDSL BOOL BOOL
  YulAND    :: YulDSL (BOOL ⊗ BOOL) BOOL
  YulOR     :: YulDSL (BOOL ⊗ BOOL) BOOL
  -- - Num Types
  YulNumNeg :: forall a. YulNum a => YulDSL a a
  YulNumAdd :: forall a. YulNum a => YulDSL (a ⊗ a) a
  -- | Number comparison with a three-way boolean-switches (LT, EQ, GT).
  YulNumCmp :: forall a. YulNum a => (BOOL, BOOL, BOOL) -> YulDSL (a ⊗ a) BOOL
  -- - Contract ABI Serialization
  YulAbiEnc :: YulObj a => YulDSL a BYTES
  YulAbiDec :: YulObj a => YulDSL BYTES (Maybe a)

  -- Storage Primitives
  --
  YulSGet :: forall a. YulVal a => YulDSL ADDR a
  YulSPut :: forall a. YulVal a => YulDSL (ADDR ⊗ a) ()

-- | Internal function object.
data Fn a b = Defun String (YulDSL a b) deriving Show

instance Category YulDSL where
  type Obj YulDSL = YulObj
  id  = YulId
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
