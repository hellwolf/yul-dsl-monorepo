{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.BuiltIns.ValueType (exports) where
-- text
import qualified Data.Text.Lazy                               as T
--
import           YulDSL.CodeGens.Yul.Internal.BuiltInRegistra

raw_boolean_operators =
  [ const_builtin  "eq" ""
  , const_builtin  "gt" ""
  , const_builtin  "lt" ""
  , const_builtin  "le" "function le(a, b) -> r { r := iszero(gt(a, b)) }"
  , const_builtin  "ge" "function ge(a, b) -> r { r := iszero(lt(a, b)) }"
  , const_builtin "sgt" ""
  , const_builtin "slt" ""
  , const_builtin "sle" "function sle(a, b) -> r { r := iszero(sgt(a, b)) }"
  , const_builtin "sge" "function sge(a, b) -> r { r := iszero(slt(a, b)) }"
  ]

cleanup_uintn = mk_builtin "__cleanup_t_uint" $ \part full ->
  let nbits = read part :: Int
      maxVal = "0x" <> replicate (nbits `div` 4) 'f'
  in (T.unlines
      [ "function " <> T.pack full <> "(value) -> cleaned {"
      , if nbits < 256
        then "  cleaned := and(value, " <> T.pack maxVal <> ")"
        else "  cleaned := value"
      , "}"
      ], [])

cleanup_intn = mk_builtin "__cleanup_t_int" $ \part full ->
  let nbits = read part :: Int
      i = show (nbits `div` 8 - 1)
  in (T.unlines
     [ "function " <> T.pack full <> "(value) -> cleaned {"
      , if nbits < 256
        then "  cleaned := signextend(" <> T.pack i <> ", value)"
        else "  cleaned := value"
     , "}"
     ], [])

exports =
  raw_boolean_operators ++
  [ cleanup_uintn
  , cleanup_intn
  ]
