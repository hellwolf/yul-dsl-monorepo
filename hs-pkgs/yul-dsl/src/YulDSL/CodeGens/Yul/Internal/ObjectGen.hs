{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.ObjectGen (compile_object) where

import           GHC.Stack                                   (HasCallStack)
--
import           Control.Monad                               (join)
import           Data.Function                               ((&))
import           Data.Functor                                ((<&>))
import           Data.List                                   (nub)
import           Data.Maybe                                  (catMaybes)
-- text
import qualified Data.Text.Lazy                              as T
--
import           YulDSL.Core
--
import           YulDSL.CodeGens.Yul.Internal.ABICodecGen
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters
import           YulDSL.CodeGens.Yul.Internal.CodeGen
import           YulDSL.CodeGens.Yul.Internal.FunctionGen


compile_fn_dispatcher :: HasCallStack
                      => Indenter -> ScopedFn -> CGState (Maybe (Code, ABICodecGen, ABICodecGen))
compile_fn_dispatcher ind (ExternalFn _ sel@(SELECTOR (_, Just (FuncSig (fname, _)))) (_ :: FnCat eff a b)) = do
  vars_a <- cg_mk_let_vars @a
  vars_b <- cg_mk_let_vars @b
  io_vars <- cg_declare_vars
  let ca = ABICodecDispatcher @a
      cb = ABICodecDispatcher @b
  let code = cbracket ind ("case " <> T.pack (show sel)) $ \ ind' ->
        maybe "" ind' io_vars <>
        -- call abi decoder
        ind' ( T.intercalate "," vars_a <> " := " <>
               -- skip selector and (TODO) check if calldatasize is big enough
               abi_decoder_name ca <> "(4, calldatasize())"
             ) <>
        -- call the function
        ind' ( T.intercalate "," vars_b <> " := " <>
               T.pack fname <> "(" <> T.intercalate "," vars_a <> ")"
             ) <>
        ind' "let memPos := __allocate_unbounded()" <>
        -- call abi encoder
        ind' ( "let memEnd := " <>
               abi_encoder_name cb <> "(memPos, " <> T.intercalate "," vars_b <> ")"
             ) <>
        ind' "return(memPos, sub(memEnd, memPos))"

  pure (Just (code, ca, cb))
compile_fn_dispatcher _ (ExternalFn _ (SELECTOR (_, Nothing)) _) = pure Nothing
compile_fn_dispatcher _ (LibraryFn _) = pure Nothing

compile_dispatchers :: HasCallStack
                    => Indenter -> [ScopedFn] -> CGState Code
compile_dispatchers ind fns = cbracket_m ind "/* dispatcher */" $ \ind' -> do
  externalFunctions <- mapM (compile_fn_dispatcher ind') fns
                       <&> catMaybes
  let code_cases = externalFunctions
                   & map (\(a, _, _) -> a)
                   & T.intercalate ""
      code_abicodecs = externalFunctions
                       & map (\(_, b, c) -> abi_codec_deps b <> abi_codec_deps c)
                       & join
                       & nub
                       & map (`abi_codec_code` ind')
                       & T.intercalate "\n"

  cg_use_builtin "__dispatcher_dependencies"

  pure $
    ind' "switch selector()" <>
    code_cases <>
    ind' "default { revert(0, 0) }" <>
    code_abicodecs

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

        builtin_codes <- cg_gen_builtin_codes

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

    pure $ T.intercalate "\n" [code_ctor, code_runtime, code_subobjs]
