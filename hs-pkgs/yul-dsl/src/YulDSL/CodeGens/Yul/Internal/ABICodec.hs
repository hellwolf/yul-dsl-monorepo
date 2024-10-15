{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.ABICodec where

-- base
import           Data.Proxy                                  (Proxy (Proxy))
-- text
import qualified Data.Text.Lazy                              as T
--
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters
import           YulDSL.CodeGens.Yul.Internal.CodeGen
import           YulDSL.Core


data ABICodec = forall a. ABIType a => MkABICodec (Proxy a)

instance Eq ABICodec where
  (MkABICodec (Proxy @a)) == (MkABICodec (Proxy @b)) = abi_type_uniq_name @a == abi_type_uniq_name @b

abi_codec_name :: ABICodec -> T.Text
abi_codec_name (MkABICodec (Proxy @a)) = T.pack (abi_type_uniq_name @a)

abi_encoder_name :: ABICodec -> T.Text
abi_encoder_name c = "__abienc_" <> abi_codec_name c

abi_decoder_name :: ABICodec -> T.Text
abi_decoder_name c = "__abidec_" <> abi_codec_name c

abi_code_def :: ABICodec -> Indenter -> T.Text
abi_code_def c@(MkABICodec (Proxy @a)) ind =
  ind ("//" <> T.pack (abi_type_uniq_name @a)) <>
  ind ("function " <> abi_encoder_name c <> "(" <> T.intercalate ", " vars <> ") {\n") <>
  ind "}\n" <>
  ind "" <>
  ind ("function " <> abi_decoder_name c <> "()" <>
       (case vars of [] -> ""; _ -> " -> " <> T.intercalate ", " vars <> "") <>
       " {\n"
      ) <>
  ind "}\n"
  where vars = gen_vars (Proxy @a)
