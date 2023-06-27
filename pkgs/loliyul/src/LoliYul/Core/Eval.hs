{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds     #-}

module LoliYul.Core.Eval where

import           Data.ByteString     (ByteString)
import qualified Data.Map            as M

import           LoliYul.Core.Types
import           LoliYul.Core.YulDSL (YulDSL (..), YulObj)

encYulObj :: YulObj a => a -> ByteString
encYulObj = undefined

decYulObj :: YulObj a => ByteString -> a
decYulObj = undefined

data EvalState = EvalState { store_map :: M.Map ADDR SVALUE
                           }
               deriving Show

initEvalState :: EvalState
initEvalState = EvalState { store_map = M.empty
                          }

evalYulDSL :: EvalState -> YulDSL a b -> a -> (EvalState, b)
evalYulDSL s YulId             a  = (s, a)
evalYulDSL s YulCoerce         a  = (s, decYulObj . encYulObj $ a)
evalYulDSL s (YulComp n m)     a  = (s'', c) where (s' , b) = evalYulDSL s  m a
                                                   (s'', c) = evalYulDSL s' n b
evalYulDSL s (YulProd m n) (a, b) = (s'', (c, d)) where (s',  c) = evalYulDSL s  m a
                                                        (s'', d) = evalYulDSL s' n b
evalYulDSL s  YulSwap      (a, b) = (s, (b, a))
evalYulDSL s  YulDis           _  = (s, ())
evalYulDSL s  YulDup           a  = (s, (a, a))
evalYulDSL s (YulConst b)      _  = (s, b)
evalYulDSL s  YulNumNeg       a   = (s, negate a)
evalYulDSL s  YulNumAdd    (a, b) = (s, a + b)
evalYulDSL s  YulSGet          r  = (s, case M.lookup r (store_map s) of
                                          Just a  -> from_svalue a
                                          Nothing -> from_svalue def_sval)
evalYulDSL s  YulSPut      (r, a) = (s', ()) where s' = s { store_map = M.insert r (to_svalue a) (store_map s) }
evalYulDSL s  (YulInternFn _ f) a = evalYulDSL s f a
