{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.ObjectGen (compile_object) where

import           GHC.Stack                                   (HasCallStack)
--
import           Control.Monad                               (join)
import           Data.Function                               ((&))
import           Data.Functor                                ((<&>))
import           Data.Maybe                                  (catMaybes)
import           Data.Typeable                               (Proxy (..))
--
import qualified Data.Text.Lazy                              as T
--
import           YulDSL.Core
--
import           Data.List                                   (nub)
import           YulDSL.CodeGens.Yul.Internal.ABICodecGen
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters
import           YulDSL.CodeGens.Yul.Internal.CodeGen
import           YulDSL.CodeGens.Yul.Internal.FunctionGen

compile_fn_dispatcher :: HasCallStack => Indenter -> ScopedFn -> CGState (Maybe (Code, ABICodec, ABICodec))
compile_fn_dispatcher ind (ExternalFn _ sel@(SELECTOR (_, Just (FuncSig (fname, _)))) (_ :: FnCat a b)) = do
  vars_a <- mk_let_vars (Proxy @a)
  vars_b <- mk_let_vars (Proxy @b)
  io_vars <- declare_vars
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
        ind' "let memPos := allocate_unbounded()" <>
        -- call abi encoder
        ind' ( "let memEnd := " <>
               abi_encoder_name cb <> "(memPos, " <> T.intercalate "," vars_b <> ")"
             ) <>
        ind' "return(memPos, sub(memEnd, memPos))"
  pure (Just (code, ca, cb))
compile_fn_dispatcher _ (ExternalFn _ (SELECTOR (_, Nothing)) _) = pure Nothing
compile_fn_dispatcher _ (LibraryFn _) = pure Nothing

compile_dispatchers :: HasCallStack => Indenter -> [ScopedFn] -> CGState Code
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
  pure $
    ind' "switch selector()" <>
    code_cases <>
    ind' "default { revert(0, 0) }" <>
    cbracket1 ind' "function selector() -> s"
      "s := div(calldataload(0), 0x100000000000000000000000000000000000000000000000000000000)" <>
    "\n" <>
    code_abicodecs

compile_object :: HasCallStack => Indenter -> YulObject -> CGState Code
compile_object ind (MkYulObject { yulObjectName = oname
                                , yulObjectCtor = ctor
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
        pure $
          code_dispatcher <>
          T.intercalate "\n" code_fns <>
          -- additional helpers, TODO move the code
          ind''' "" <>
          cbracket1 ind''' "function allocate_unbounded() -> memPtr"
            "memPtr := mload(64)"


    -- sub objects code
    code_subobjs <- mapM (compile_object ind') subobjs <&> T.intercalate "\n"

    pure $ T.intercalate "\n" [code_ctor, code_runtime, code_subobjs]
