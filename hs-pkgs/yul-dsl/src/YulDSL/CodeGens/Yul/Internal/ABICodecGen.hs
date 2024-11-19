{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
module YulDSL.CodeGens.Yul.Internal.ABICodecGen
  ( ABICodecGen (..)
  , abi_encoder_name
  , abi_decoder_name
  , abi_codec_deps
  , abi_codec_code )where

-- text
import qualified Data.Text.Lazy                              as T
--
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters
import           YulDSL.CodeGens.Yul.Internal.CodeGen
import           YulDSL.Core


data ABICodecGen where
  ABICodecDispatcher :: forall a. ABITypeable a => ABICodecGen
  ABICodecStack :: ABICoreType -> ABICodecGen

instance Show ABICodecGen where
  show (ABICodecDispatcher @a) = "_dispatcher_" <> abiTypeCompactName @a
  show (ABICodecStack a)       = "_stack_" <> abiCoreTypeCompactName a

instance Eq ABICodecGen where
  a == b = show a == show b

abi_codec_name_base :: ABICodecGen -> Code
abi_codec_name_base c = T.pack (show c)

abi_encoder_name :: ABICodecGen -> Code
abi_encoder_name c = "__abienc" <> abi_codec_name_base c

abi_decoder_name :: ABICodecGen -> Code
abi_decoder_name c = "__abidec" <> abi_codec_name_base c

abi_codec_deps :: ABICodecGen -> [ABICodecGen]
abi_codec_deps c@(ABICodecDispatcher @a) = fmap ABICodecStack (abiTypeInfo @a) <> [c]
abi_codec_deps c@(ABICodecStack _)       = [c]

abi_codec_code :: ABICodecGen -> Indenter -> T.Text
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
    ans = abiTypeInfo @a
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

abi_decoder_dispatcher_code :: [ABICoreType] -> [Var] -> Indenter -> T.Text
abi_decoder_dispatcher_code ans vars ind =
  ind "// TODO size check if slt(sub(end - offset), ..) { .. }" <>
  go ans 0
  where go :: [ABICoreType] -> Int -> Code
        go (n:ns) slot = cbracket ind ""
          (\ind' ->
             ind' ("let offset := " <> T.pack(show (slot * 32))) <>
             ind' (vars !! slot <> " := " <> abi_decoder_name (ABICodecStack n) <> "(add(headStart, offset), dataEnd)")
          ) <>
          go ns (slot + 1)
        go [] _ = ""

abi_decoder_stack_code :: ABICoreType -> Var -> Indenter -> T.Text
abi_decoder_stack_code a ret ind = go a where
  go (INTx' _ _) = ind (ret <> " := calldataload(offset)")
  go _           = ind "// TODO abi_decoder_code_final"

abi_encoder_dispatcher_code :: [ABICoreType] -> [Var] -> Indenter -> T.Text
abi_encoder_dispatcher_code ans vars ind =
  ind ("tail := add(headStart, " <> T.pack (show dataSize) <> ")") <>
  go ans 0
  where go :: [ABICoreType] -> Int -> Code
        go (n:ns) slot = ind (abi_encoder_name (ABICodecStack n) <> "(" <>
                              "add(headStart, " <> T.pack(show (slot * 32)) <> "), " <>
                              vars !! slot <> ")"
                             ) <>
          go ns (slot + 1)
        go _ _         = ""
        dataSize = length ans * 32

abi_encoder_stack_code :: ABICoreType -> Var -> Indenter -> T.Text
abi_encoder_stack_code a var ind = go a where
  go (INTx' _ _) = ind ("mstore(pos, " <> var <> ")")
  go _           = ind "// TODO abi_encoder_code"
