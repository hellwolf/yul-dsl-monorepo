{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.ObjectGen (compile_object) where

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
import           YulDSL.CodeGens.Yul.Internal.ABICodec
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters
import           YulDSL.CodeGens.Yul.Internal.CodeGen
import           YulDSL.CodeGens.Yul.Internal.FunctionGen

compile_fn_dispatcher :: Indenter -> ScopedFn -> CGState (Maybe (Code, ABICodec, ABICodec))
compile_fn_dispatcher ind (ExternalFn _ sel@(SEL (_, Just (fname, _))) (_ :: Fn a b)) = do
  vars_a <- mk_let_vars (Proxy @a)
  vars_b <- mk_let_vars (Proxy @b)
  io_vars <- declare_vars
  let ca = MkABICodec (Proxy @a)
      cb = MkABICodec (Proxy @b)
  let code = cbracket ind ("case " <> T.pack (show sel)) $ \ ind' ->
        (case io_vars of Nothing -> ""; Just io_vars' -> ind' io_vars') <>
        ind' "// TODO, abi decoding of input" <>
        ind' (T.intercalate "," vars_a <> " := " <> abi_decoder_name ca <> "()") <>
        ind' ( T.intercalate "," vars_b <> " := " <>
               T.pack fname <> "(" <> T.intercalate "," vars_a <> ")"
             ) <>
        ind' "// TODO, abi encoding of output" <>
        ind' (abi_encoder_name cb <> "(" <> T.intercalate "," vars_b <> ")")
  pure (Just (code, ca, cb))
compile_fn_dispatcher _ (ExternalFn _ (SEL (_, Nothing)) _) = pure Nothing
compile_fn_dispatcher _ (LibraryFn _) = pure Nothing

compile_dispatchers :: Indenter -> [ScopedFn] -> CGState Code
compile_dispatchers ind fns = cbracket_m ind "/* dispatcher */" $ \ind' -> do
  externalFunctions <- sequence (map (compile_fn_dispatcher ind') fns)
                       <&> catMaybes
  let code_cases = externalFunctions
                   & map (\(a, _, _) -> a)
                   & T.intercalate ""
      code_abicodecs = externalFunctions
                       & map (\(_, b, c) -> [b, c])
                       & join
                       & nub
                       & map (flip abi_code_def ind')
                       & T.intercalate "\n"
  pure $
    ind' "switch selector()" <>
    code_cases <>
    ind' "default { revert(0, 0) }" <>
    ( cbracket1 ind'
      "function selector() -> s"
      "s := div(calldataload(0), 0x100000000000000000000000000000000000000000000000000000000)"
    ) <>
    "\n" <>
    code_abicodecs

compile_object :: Indenter -> YulObject -> CGState Code
compile_object ind (MkYulObject { yulObjectName = oname
                                , yulObjectCtor = ctor
                                , yulObjectSFns = sfns
                                , yulSubObjects = subobjs
                                }) = do
  cbracket_m ind ("object \"" <> T.pack oname <> "\"") $ \ind' -> do
    -- object init code
    code_ctor <- cbracket_m ind' "code /* object init code */" $ \ind'' -> do
      user_ctor <- compile_cat ind'' ctor ([], [])
      pure $
        ind'' "datacopy(0, dataoffset(\"runtime\"), datasize(\"runtime\"))" <>
        ind'' "" <>
        ind'' "// constructor" <>
        user_ctor <>
        ind'' "" <>
        ind'' "return(0, datasize(\"runtime\"))"

    -- object runtime
    code_runtime <- cbracket_m ind' ("object \"runtime\"") $ \ind'' -> do
      cbracket_m ind'' "code" $ \ind''' -> do
        -- dispatcher
        code_dispatcher <- compile_dispatchers ind''' sfns
        -- exported functions
        code_fns <- mapM (compile_scoped_fn ind''') sfns
        pure $
          code_dispatcher <>
          T.intercalate "\n" code_fns

    -- sub objects code
    code_subobjs <- mapM (compile_object ind') subobjs <&> T.intercalate "\n"

    pure $ T.intercalate "\n" [code_ctor, code_runtime, code_subobjs]
