{-|

Copyright   : (c) 2023-2024 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : hellwolf@yolc.dev
Stability   : experimental

= Description

This module provides a system for evaluation of a YulCat morphism semantically.

-}

module YulDSL.Eval where

-- base
import           Data.Maybe               (fromJust)
import           GHC.Stack                (HasCallStack)
-- containers
import qualified Data.Map                 as M
-- mtl
import           Control.Monad.State.Lazy (State, evalState, gets, modify')
-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.Fn
import           YulDSL.Core.YulCat
import           YulDSL.Core.YulCatObj


newtype EvalData = MkEvalData { store_map :: M.Map ADDR WORD
                              }
                 deriving Show

type EvalState = State EvalData

initEvalState :: EvalData
initEvalState = MkEvalData { store_map = M.empty
                           }

evalYulCat' :: (HasCallStack, YulO2 a b) => YulCat eff a b -> a -> EvalState b
-- type-level coercion
evalYulCat' YulCoerce a = pure $ fromJust . abiDecode . abiEncode $ a
-- category
evalYulCat' YulSplit  a = pure $ fromJust . abiDecode . abiEncode $ a
evalYulCat' YulId     a = pure a
evalYulCat' (YulComp n m) a = evalYulCat' m a >>= evalYulCat' n
-- monoidal category
evalYulCat' (YulProd m n) (a, b) = do
  a' <- evalYulCat' m a
  b' <- evalYulCat' n b
  pure (a', b')
evalYulCat' YulSwap (a, b) = pure (b, a)
-- cartesian category
evalYulCat' (YulFork m n) a = do
  b <- evalYulCat' m a
  c <- evalYulCat' n a
  pure (b, c)
evalYulCat' YulExl  (a, _) = pure a
evalYulCat' YulExr  (_, b) = pure b
evalYulCat' YulDis  _  = pure ()
evalYulCat' YulDup  a  = pure (a, a)
-- control flow
evalYulCat' (YulJump _ f) a = evalYulCat' f a
-- value functions
evalYulCat' (YulEmbed b)  _ = pure b
evalYulCat' YulNumNeg a = pure (negate a)
evalYulCat' YulNumAdd (a, b) = pure (a + b)
evalYulCat' YulSGet r = gets $ \s -> fromJust (fromWord =<< M.lookup r (store_map s))
evalYulCat' YulSPut (r, a) = modify' $ \s -> s { store_map = M.insert r (toWord a) (store_map s) }
-- TODOs:
evalYulCat' c _ = error ("evalYulCat: " <> show c)

evalYulCat :: YulO2 a b => YulCat eff a b -> a -> b
evalYulCat s a = evalState (evalYulCat' s a) initEvalState

evalFn :: forall eff f. YulO2 (NP (UncurryNP'Fst f)) (UncurryNP'Snd f)
       => Fn eff f -> NP (UncurryNP'Fst f) -> UncurryNP'Snd f
evalFn = evalYulCat . fnCat . unFn
