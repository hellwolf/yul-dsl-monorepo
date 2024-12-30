{-|

Copyright   : (c) 2023-2024 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : hellwolf@yolc.dev
Stability   : experimental

= Description

Generate solidity/yul code for Fn, YulCat, and YulObject.

-}

module YulDSL.CodeGens.YulGen
  ( compileFn
  , compileYulObject
  ) where

import YulDSL.Core                                 (FnCat, YulO2, YulObject)
--
import YulDSL.CodeGens.Yul.Internal.BuiltIns       (default_builtins, prelude_builtins)
import YulDSL.CodeGens.Yul.Internal.CodeFormatters (Code, init_ind)
import YulDSL.CodeGens.Yul.Internal.CodeGen        (cg_register_builtin, cg_use_builtin, gen_code)
import YulDSL.CodeGens.Yul.Internal.FunctionGen    (compile_fn)
import YulDSL.CodeGens.Yul.Internal.ObjectGen      (compile_object)


-- | Compiling a yul function.
compileFn :: forall eff a b. YulO2 a b => FnCat eff a b -> Code
compileFn fn = gen_code $ do
  mapM_ cg_register_builtin default_builtins
  mapM_ cg_use_builtin prelude_builtins
  compile_fn init_ind fn

-- | Compiling the yul object.
compileYulObject :: YulObject -> Code
compileYulObject obj = gen_code $ do
  mapM_ cg_register_builtin default_builtins
  mapM_ cg_use_builtin prelude_builtins
  compile_object init_ind obj
