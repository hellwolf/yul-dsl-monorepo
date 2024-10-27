{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
module YulDSL.CodeGens.Yul.Internal.ABICodec where

-- text
import qualified Data.Text.Lazy                              as T
--
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters
import           YulDSL.CodeGens.Yul.Internal.CodeGen
import           YulDSL.Core


data ABICodec where
  ABICodecDispatcher :: forall a. ABIType a => ABICodec
  ABICodecStack :: ABITypeInfo -> ABICodec

instance Show ABICodec where
  show (ABICodecDispatcher @a) = "_dispatcher_" <> abi_type_uniq_name @a
  show (ABICodecStack a)       = "_stack_" <> abi_type_uniq_name' a

instance Eq ABICodec where
  a == b = show a == show b

abi_codec_name :: ABICodec -> Code
abi_codec_name c = T.pack (show c)

abi_encoder_name :: ABICodec -> Code
abi_encoder_name c = "__abienc" <> T.pack (show c)

abi_decoder_name :: ABICodec -> Code
abi_decoder_name c = "__abidec" <> T.pack (show c)

abi_codec_deps :: ABICodec -> [ABICodec]
abi_codec_deps c@(ABICodecDispatcher @a) = fmap ABICodecStack (abi_type_info @a) <> [c]
abi_codec_deps c@(ABICodecStack _)       = [c]

abi_codec_code :: ABICodec -> Indenter -> T.Text
abi_codec_code c@(ABICodecDispatcher @a) ind =
  if not (null vars) then
    ind ("// ABICodecDispatcher " <> T.pack (show c)) <>
    ind ("function " <> abi_decoder_name c <> "(headStart, dataEnd)" <>
         (case vars of [] -> ""; _ -> " -> " <> T.intercalate ", " vars <> "") <> " {"
        ) <>
    abi_decoder_dispatcher_code ans vars (indent ind) <>
    ind "}\n" <>
    ind ("function " <> abi_encoder_name c <> "(headStart, " <> T.intercalate ", " vars <> ") -> tail {") <>
    abi_encoder_dispatcher_code ans vars (indent ind) <>
    ind "}\n"
  else "" -- no code for the unit type
  where
    ans = abi_type_info @a
    vars = gen_vars (length ans)
abi_codec_code c@(ABICodecStack a) ind =
    ind ("// ABICodecStack " <> T.pack (show c)) <>
    ind ("function " <> abi_decoder_name c <> "(offset, end) -> " <> var <> " {") <>
    abi_decoder_stack_code a var (indent ind) <>
    ind "}\n" <>
    ind ("function " <> abi_encoder_name c <> "(pos, " <> var <> ") {") <>
    abi_encoder_stack_code a var (indent ind) <>
    ind "}\n"
  where
    var = gen_vars 1 !! 0

abi_decoder_dispatcher_code :: [ABITypeInfo] -> [Var] -> Indenter -> T.Text
abi_decoder_dispatcher_code ans vars ind =
  ind "// TODO size check if slt(sub(end - offset), ..) { .. }" <>
  go ans 0
  where go :: [ABITypeInfo] -> Int -> Code
        go (n:ns) slot = cbracket ind ""
          (\ind' ->
             ind' ("let offset := " <> T.pack(show (slot * 32))) <>
             ind' (vars !! slot <> " := " <> abi_decoder_name (ABICodecStack n) <> "(add(headStart, offset), dataEnd)")
          ) <>
          go ns (slot + 1)
        go [] _ = ""

abi_decoder_stack_code :: ABITypeInfo -> Var -> Indenter -> T.Text
abi_decoder_stack_code a ret ind = go a where
  go (INTx' _ _) = ind (ret <> " := calldataload(offset)")
  go _           = ind "// TODO abi_decoder_code_final"

abi_encoder_dispatcher_code :: [ABITypeInfo] -> [Var] -> Indenter -> T.Text
abi_encoder_dispatcher_code ans vars ind =
  ind ("tail := add(headStart, " <> T.pack (show dataSize) <> ")") <>
  go ans 0
  where go :: [ABITypeInfo] -> Int -> Code
        go (n:ns) slot = ind (abi_encoder_name (ABICodecStack n) <> "(" <>
                              "add(headStart, " <> T.pack(show (slot * 32)) <> "), " <>
                              vars !! slot <> ")"
                             ) <>
          go ns (slot + 1)
        go _ _         = ""
        dataSize = length ans * 32

abi_encoder_stack_code :: ABITypeInfo -> Var -> Indenter -> T.Text
abi_encoder_stack_code a var ind = go a where
  go (INTx' _ _) = ind ("mstore(pos, " <> var <> ")")
  go _           = ind "// TODO abi_encoder_code"
