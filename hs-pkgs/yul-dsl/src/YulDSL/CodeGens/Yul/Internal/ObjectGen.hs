{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.ObjectGen (compile_object) where

-- base
import Control.Monad                               (unless)
import Data.Functor                                ((<&>))
import Data.Maybe                                  (catMaybes)
-- text
import Data.Text.Lazy                              qualified as T
--
import YulDSL.Core
--
import YulDSL.CodeGens.Yul.Internal.CodeFormatters
import YulDSL.CodeGens.Yul.Internal.CodeGen
import YulDSL.CodeGens.Yul.Internal.FunctionGen
import YulDSL.CodeGens.Yul.Internal.Variable


compile_fn_dispatcher :: HasCallStack
                      => Indenter -> ScopedFn -> CGState (Maybe Code)
compile_fn_dispatcher ind (ExternalFn _ sel@(SELECTOR (_, Just (FuncSig (fname, _)))) (_ :: FnCat eff a b)) = do
  let abidec_builtin = "__abidec_dispatcher_" <> abiTypeCompactName @a
      abienc_builtin = "__abienc_dispatcher_" <> abiTypeCompactName @b
  vars_a <- cg_create_vars @a
  vars_b <- cg_create_vars @b
  unless (null vars_a) $ cg_use_builtin abidec_builtin
  unless (null vars_b) $ cg_use_builtin abienc_builtin
  pure . Just $ cbracket ind ("case " <> T.pack (show sel)) $ \ ind' ->
        declare_vars ind' (vars_a ++ vars_b) <>
        -- call the abi decoder for inputs
        ( if not (null vars_a)
          then ind' ( spread_vars vars_a <> " := " <> T.pack abidec_builtin <> "(4, calldatasize())" )
          else ""
        ) <>
        -- call the function
        ind' ( (if not (null vars_b) then spread_vars vars_b <> " := " else "") <>
               T.pack fname <> "(" <> spread_vars vars_a <> ")"
             ) <>
        -- call the abi decoder for outputs
        ( if not (null vars_b) then
            ind' "let memPos := __allocate_unbounded()" <>
            ind' ( "let memEnd := " <> T.pack abienc_builtin <> "(memPos, " <> spread_vars vars_b <> ")" ) <>
            ind' "return(memPos, sub(memEnd, memPos))"
          else ind' "return(0, 0)"
        )
compile_fn_dispatcher _ _ = pure Nothing

compile_dispatchers :: HasCallStack
                    => Indenter -> [ScopedFn] -> CGState Code
compile_dispatchers ind fns = cbracket_m ind "/* dispatcher */" $ \ind' -> do
  cases <- mapM (compile_fn_dispatcher ind') fns
           <&> catMaybes

  cg_use_builtin "__dispatcher_dependencies"

  pure $
    ind' "switch selector()" <>
    T.intercalate "" cases <>
    ind' "default { revert(0, 0) }"

-- | Compile dependencies with a function id filter @fidFilter@.
compile_deps :: HasCallStack
             => Indenter -> (String -> Bool) -> CGState [Code]
compile_deps ind fidFilter = do
  deps <- fmap (\(i, c) -> case c of (MkAnyYulCat cat) -> MkAnyFnCat (MkFnCat i cat))
          . filter (\(i, _) -> fidFilter i)
          <$> cg_list_dependent_cats
  mapM (\case (MkAnyFnCat f) -> compile_fn ind f) deps

compile_object :: HasCallStack
               => Indenter -> YulObject -> CGState Code
compile_object ind (MkYulObject { yulObjectName = oname
                                , yulObjectCtor = (MkAnyYulCat ctor)
                                , yulObjectSFns = sfns
                                , yulSubObjects = subobjs
                                }) = do
  cbracket_m ind ("object \"" <> T.pack oname <> "\"") $ \ind' -> do
    -- object init code
    code_ctor <- cbracket_m ind' "code /* object init code */" $ \ind'' -> do
      user_ctor <- compile_cat (indent ind'') ctor ([], [])
      pure $
        ind'' "datacopy(0, dataoffset(\"runtime\"), datasize(\"runtime\"))" <>
        ind'' "" <>
        ind'' "// constructor" <>
        cbracket1 ind'' "" user_ctor <>
        ind'' "" <>
        ind'' "return(0, datasize(\"runtime\"))"

    -- object runtime
    code_runtime <- cbracket_m ind' "object \"runtime\"" $ \ind'' -> do
      cbracket_m ind'' "code" $ \ind''' -> do
        -- dispatcher
        code_dispatcher <- compile_dispatchers ind''' sfns

        -- exported functions
        code_fns <- mapM (compile_scoped_fn ind''') sfns

        -- dependencies
        deps_codes <- compile_deps ind (not . (`elem` map (anyFnId . unScopedFn) sfns))

        builtin_codes <- cg_gen_builtin_codes ind'''

        pure $
          code_dispatcher <>

          ind''' "// exported functions" <>
          T.intercalate "\n" code_fns <>
          "\n" <>

          ind''' "// dependent functions" <>
          T.intercalate "\n" deps_codes <>
          "\n" <>

          ind''' "// builtin functions" <>
          T.intercalate "\n" builtin_codes

    -- sub objects code
    code_subobjs <- mapM (compile_object ind') subobjs <&> T.intercalate "\n"

    cg_reset_for_object

    pure $ T.intercalate "\n" [code_ctor, code_runtime, code_subobjs]
