{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.BuiltIns.ABICodec (exports) where
-- base
--
import           YulDSL.Core
-- text
import qualified Data.Text.Lazy                               as T
--
import           YulDSL.CodeGens.Yul.Internal.BuiltInRegistra
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters
import           YulDSL.CodeGens.Yul.Internal.Variables


abidec_dispatcher = mk_builtin "__abidec_dispatcher_" $ \part full ->
  let types = decodeAbiCoreTypeCompactName part
      vars  = gen_vars (length types)
  in ( [ "function " <> T.pack full <> "(headStart, dataEnd) -> " <> T.intercalate ", " vars <> " {" ]
       ++ abidec_dispatcher_body types vars ++
       [ "}" ]
     , fmap abidec_from_stack_builtin_name types
     )

abienc_dispatcher = mk_builtin "__abienc_dispatcher_" $ \part full ->
  let types = decodeAbiCoreTypeCompactName part
      vars  = gen_vars (length types)
  in ( [ "function " <> T.pack full <> "(headStart, " <> T.intercalate ", " vars <>") -> tail {" ]
       ++ abienc_dispatcher_types types vars ++
       [ "}" ]
     , fmap abienc_from_stack_builtin_name types
     ++ fmap value_cleanup_builtin_name types
     )

abidec_from_stack = mk_builtin "__abidec_from_stack_" $ \part full ->
  let t = case decodeAbiCoreTypeCompactName part of
            [x] -> x
            _   -> error ("abidec_from_stack, bad part: " <> part)
      go (INTx' _ _) = [ "value := calldataload(offset)" ]
      go (BOOL')     = [ "value := calldataload(offset)" ]
      go _           = [ "// TOOD: "  <> T.pack part ]
  in ( [ "function " <> T.pack full <> "(offset, end) -> value {"
       , " // TODO: validator_revert_t_int128(value)"
       ]
       ++ fmap add_indent (go t) ++
       [ "}" ]
     , [])

abienc_from_stack = mk_builtin "__abienc_from_stack_" $ \part full ->
  let t = case decodeAbiCoreTypeCompactName part of
            [x] -> x
            _   -> error ("abienc_from_stack, bad part: " <> part)
      go (INTx' _ _) = [ "mstore(pos, " <> T.pack (value_cleanup_builtin_name t) <> "(value))" ]
      go (BOOL')     = [ "mstore(pos, " <> T.pack (value_cleanup_builtin_name t) <> "(value))" ]
      go _           = [ "// TOOD: "  <> T.pack part ]
  in ( [ "function " <> T.pack full <> "(pos, value) {" ]
       ++ fmap add_indent (go t) ++
       [ "}" ]
     , [])

exports :: [BuiltInEntry]
exports =
  [ abidec_dispatcher
  , abienc_dispatcher
  , abidec_from_stack
  , abienc_from_stack
  ]

--
-- internal functions
--

abidec_from_stack_builtin_name t = "__abidec_from_stack_" ++ abiCoreTypeCompactName t

abidec_dispatcher_body :: [ABICoreType] -> [Var] -> [Code]
abidec_dispatcher_body types vars =
  add_indent "if slt(sub(dataEnd, headStart), " <> T.pack (show dataSize) <> ") { revert(0, 0) }" :
  map add_indent (go types 0)
  where
    go (n:ns) slot =
      [ "{"
      , add_indent $ "let offset := " <> T.pack (show (slot * 32))
      , add_indent $ vars !! slot <> " := "
        <> T.pack (abidec_from_stack_builtin_name n) <> "(add(headStart, offset), dataEnd)"
      , "}"
      ]
      ++ go ns (slot + 1)
    go [] _ = []
    dataSize = length types * 32

abienc_from_stack_builtin_name t = "__abienc_from_stack_" ++ abiCoreTypeCompactName t

abienc_dispatcher_types :: [ABICoreType] -> [Var] -> [Code]
abienc_dispatcher_types types vars =
  add_indent "tail := add(headStart, " <> T.pack (show dataSize) <> ")" :
  map add_indent (go types 0)
  where
    go (n:ns) slot =
      T.pack (abienc_from_stack_builtin_name n)
      <> "(add(headStart, " <> T.pack(show (slot * 32)) <> "), " <> vars !! slot <> ")"
      : go ns (slot + 1)
    go _ _         = []
    dataSize = length types * 32

value_cleanup_builtin_name t = "__cleanup_t_" ++ abiCoreTypeCanonName t
