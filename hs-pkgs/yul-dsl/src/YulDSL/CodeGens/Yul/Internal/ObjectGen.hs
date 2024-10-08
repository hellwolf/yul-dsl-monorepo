{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.ObjectGen (compile_object) where

import           Data.Functor                                ((<&>))
import           Data.Maybe                                  (mapMaybe)
import           Data.Typeable                               (Proxy (..))
--
import qualified Data.Text.Lazy                              as T
--
import           YulDSL.Core
--
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters
import           YulDSL.CodeGens.Yul.Internal.CodeGen
import           YulDSL.CodeGens.Yul.Internal.FunctionGen


create_dispatcher :: Indenter -> [ScopedFn] -> CGState Code
create_dispatcher ind fns = do
  code_cases <- (mapM case_fn . mapMaybe dispatchable) fns
                <&> T.intercalate ""
  pure $
    ind "{ // Dispatcher\n" <>
    ind' "switch selector()" <>
    code_cases <>
    ind' "default { revert(0, 0) }" <>
    ind' "function selector() -> s {" <>
    ind' "  s := div(calldataload(0), 0x100000000000000000000000000000000000000000000000000000000)" <>
    ind' "}" <>
    ind "}"
  where ind' = indent ind
        dispatchable (ExternalFn _ sel fn) = Just (sel, MkAnyYulCat (fnCat fn))
        dispatchable (LibraryFn _)         = Nothing
        case_fn (sel@(SEL (sig, Just (fname, _))), MkAnyYulCat (_ :: YulCat a b)) = do
          vars_a <- mk_let_vars (Proxy @a)
          vars_b <- mk_let_vars (Proxy @b)
          io_vars <- declare_vars
          return $ cbracket ind' ("case " <> T.pack (show sel)) $ \ ind'' ->
            (case io_vars of Nothing -> ""; Just io_vars' -> ind'' io_vars') <>
            ind'' "// TODO, abi decoding of input" <>
            ind'' (T.intercalate "," vars_b <> " := " <>
                   T.pack fname <> "(" <> T.intercalate "," vars_a <> ")") <>
            ind'' "// TODO, abi encoding of output"

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
        code_dispatcher <- create_dispatcher ind''' sfns
        pure code_dispatcher
        -- exported functions
        code_fns <- mapM (compile_scoped_fn ind''') sfns
        pure $
          code_dispatcher <>
          T.intercalate "\n" code_fns

    -- sub objects code
    code_subobjs <- mapM (compile_object ind') subobjs <&> T.intercalate "\n"

    pure $ T.intercalate "\n" [code_ctor, code_runtime, code_subobjs]
