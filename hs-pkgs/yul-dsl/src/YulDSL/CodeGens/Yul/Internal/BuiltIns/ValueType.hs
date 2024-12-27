{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.BuiltIns.ValueType (exports) where
-- text
import Data.Text.Lazy                               qualified as T
--
import YulDSL.CodeGens.Yul.Internal.BuiltInRegistra

raw_boolean_operators =
  [ const_builtin  "eq" []
  , const_builtin  "gt" []
  , const_builtin  "lt" []
  , const_builtin  "le" [ "function le(a, b) -> r { r := iszero(gt(a, b)) }" ]
  , const_builtin  "ge" [ "function ge(a, b) -> r { r := iszero(lt(a, b)) }" ]
  , const_builtin "sgt" []
  , const_builtin "slt" []
  , const_builtin "sle" [ "function sle(a, b) -> r { r := iszero(sgt(a, b)) }" ]
  , const_builtin "sge" [ "function sge(a, b) -> r { r := iszero(slt(a, b)) }" ]
  ]

cleanup_bool = const_builtin "__cleanup_t_bool"
  [ "function __cleanup_t_bool(value) -> cleaned { cleaned := iszero(iszero(value)) }" ]

cleanup_uintn = mk_builtin "__cleanup_t_uint" $ \part full ->
  let nbits = read part :: Int
      maxVal = "0x" <> replicate (nbits `div` 4) 'f'
  in ( [ "function " <> T.pack full <> "(value) -> cleaned {"
       , if nbits < 256
         then " cleaned := and(value, " <> T.pack maxVal <> ")"
         else " cleaned := value"
       , "}"
       ], [])

cleanup_intn = mk_builtin "__cleanup_t_int" $ \part full ->
  let nbits = read part :: Int
      i = show (nbits `div` 8 - 1)
  in ( [ "function " <> T.pack full <> "(value) -> cleaned {"
       , if nbits < 256
         then " cleaned := signextend(" <> T.pack i <> ", value)"
         else " cleaned := value"
       , "}"
       ], [])

cleanup_address = const_builtin_with_deps "__cleanup_t_address"
  [ "function __cleanup_t_address(value) -> cleaned { cleaned := __cleanup_t_uint160(value) }" ]
  [ "__cleanup_t_uint160" ]

validate_value = mk_builtin "__validate_t_" $ \part full ->
  let cleanupf = "__cleanup_t_" <> part
  in ( [ "function " <> T.pack full <> "(value) {"
       <> " if iszero(eq(value, " <> T.pack cleanupf <> "(value))) { revert(0, 0) }"
       <> " }"
       ]
     , [ cleanupf ]
     )

exports :: [BuiltInEntry]
exports =
  raw_boolean_operators ++
  [ cleanup_bool
  , cleanup_uintn
  , cleanup_intn
  , cleanup_address
  , validate_value
  ]
