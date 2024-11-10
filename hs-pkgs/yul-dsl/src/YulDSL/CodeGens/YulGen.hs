{-# LANGUAGE GADTs #-}

module YulDSL.CodeGens.YulGen
  ( compileFn
  , compileObject
  ) where

import           YulDSL.Core                                 (FnCat, YulO2, YulObject)
--
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters (Code, init_ind)
import           YulDSL.CodeGens.Yul.Internal.CodeGen        (gen_code)
import           YulDSL.CodeGens.Yul.Internal.FunctionGen    (compile_fn)
import           YulDSL.CodeGens.Yul.Internal.ObjectGen      (compile_object)


-- | Compiling a yul function.
compileFn :: forall a b. YulO2 a b => FnCat a b -> Code
compileFn fn = gen_code (compile_fn init_ind fn)

-- | Compiling the yul object.
compileObject :: YulObject -> Code
compileObject obj = gen_code (compile_object init_ind obj)
