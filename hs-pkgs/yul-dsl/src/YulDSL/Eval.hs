{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

This module provides a function 'evalYulCat' simulating the evaluation of the 'YulDSL'.

-}

module YulDSL.Eval where

-- base
import           Data.Maybe           (fromJust)
-- containers
import qualified Data.Map             as M
-- eth-abi
import           Ethereum.ContractABI
--
import           YulDSL.Core.YulCat


{-# ANN EvalState "HLint: ignore Use newtype instead of data" #-}
data EvalState = EvalState { store_map :: M.Map ADDR WORD
                           }
               deriving Show

initEvalState :: EvalState
initEvalState = EvalState { store_map = M.empty
                          }

evalYulCat' :: YulO2 a b => EvalState -> YulCat eff a b -> a -> (EvalState, b)
evalYulCat' s YulId             a  = (s, a)
evalYulCat' s YulCoerce         a  = (s, fromJust . abiDecode . abiEncode $ a)
evalYulCat' s (YulComp n m)     a  = (s'', c) where (s' , b) = evalYulCat' s  m a
                                                    (s'', c) = evalYulCat' s' n b
evalYulCat' s (YulProd m n) (a, b) = (s'', (c, d)) where (s',  c) = evalYulCat' s  m a
                                                         (s'', d) = evalYulCat' s' n b
evalYulCat' s  YulSwap      (a, b) = (s, (b, a))
evalYulCat' s  YulDis           _  = (s, ())
evalYulCat' s  YulDup           a  = (s, (a, a))
evalYulCat' s (YulEmbed b)      _  = (s, b)
evalYulCat' s  YulNumNeg       a   = (s, negate a)
evalYulCat' s  YulNumAdd    (a, b) = (s, a + b)
evalYulCat' s  YulSGet          r  = (s, fromJust (fromWord =<< M.lookup r (store_map s)))
evalYulCat' s  YulSPut      (r, a) = (s', ()) where s' = s { store_map = M.insert r (toWord a) (store_map s) }
evalYulCat' _ _ _ = error "evalYulCat"

evalYulCat :: YulO2 a b => YulCat eff a b -> a -> b
evalYulCat s c = snd (evalYulCat' initEvalState s c)
