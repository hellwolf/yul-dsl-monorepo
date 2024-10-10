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
  return $
    "{ // Dispatcher\n" <>
    ind' "switch selector()" <>
    code_cases <>
    ind' "default { revert(0, 0) }" <>
    ind' "function selector() -> s {" <>
    ind' "  s := div(calldataload(0), 0x100000000000000000000000000000000000000000000000000000000)" <>
    ind' "}" <>
    ind "}"
  where ind' = indent ind
        ind'' = indent ind'
        dispatchable (ExternalFn _ sel fn) = Just (sel, MkAnyYulCat (fnCat fn))
        dispatchable (LibraryFn _)         = Nothing
        case_fn (sel@(SEL (sig, Just (fname, _))), MkAnyYulCat (_ :: YulCat a b)) = do
          vars_a <- mk_let_vars (Proxy @a)
          vars_b <- mk_let_vars (Proxy @b)
          code_vars <- declare_vars ind''
          return $
            ind' ("case " <> T.pack (show sel) <> "{") <>
            code_vars <>
            ind'' "// TODO, abi decoding of input" <>
            ind'' (T.intercalate "," vars_b <> " := " <>
                   T.pack fname <> "(" <> T.intercalate "," vars_a <> ")") <>
            ind' "}"

compile_object :: Indenter -> YulObject -> CGState Code
compile_object ind (MkYulObject { yulObjectName = oname
                                , yulObjectCtor = ctor
                                , yulObjectSFns = sfns
                                , yulSubObjects = subos
                                }) = do
  let ind' = indent ind
      ind'' = indent ind'
      ind''' = indent ind''
  code_ctor <- compile_cat ind'' ctor ([], [])
  code_dispatcher <- create_dispatcher ind''' sfns
  code_fns <- mapM (compile_scoped_fn ind''') sfns -- exported functions
  code_subos <- mapM (compile_object ind') subos -- sub objects
  return $
    ind ("object \"" <> T.pack oname <> "\" {") <>
    ( -- object init
      ind' "code {" <>
      (
        ind'' "datacopy(0, dataoffset(\"runtime\"), datasize(\"runtime\"))" <>
        ind'' "" <>
        ind'' "// constructor" <>
        ind'' code_ctor <>
        ind'' "return(0, datasize(\"runtime\"))"
      ) <>
      ind' "}" <>
      -- runtime object
      ind' "object \"runtime\" {" <>
      (
        ind'' "code {" <>
        ind''' code_dispatcher <>
        T.intercalate "\n" code_fns <>
        ind'' "}"
      ) <>
      ind' "}"
    ) <> "\n" <>
    T.intercalate "\n" code_subos <>
    ind "}"
