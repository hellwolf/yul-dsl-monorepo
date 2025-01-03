{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.BuiltIns.ValueType
  ( exports
  , value_cleanup_builtin_name
  , validate_value_builtin_name
  ) where
-- yul-dsl
import YulDSL.Core
-- text
import Data.Text.Lazy                               qualified as T
--
import YulDSL.CodeGens.Yul.Internal.BuiltInRegistra
import YulDSL.CodeGens.Yul.Internal.Variable

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
  let cleanup_f = "__cleanup_t_" <> part
  in ( [ "function " <> T.pack full <> "(value) {"
       <> " if neq(value, " <> T.pack cleanup_f <> "(value)) { revert(0, 0) }"
       <> " }"
       ]
     , [ cleanup_f
       ]
     )

keccak256 = mk_builtin "__keccak_c_" $ \part full ->
  let types = decodeAbiCoreTypeCompactName part
      vars  = gen_vars (length types)
      abienc_builtin = "__abienc_from_stack_c_" <> part
  in ( [ "function " <> T.pack full <> "(" <> spread_vars vars <> ") -> hash {"
       , " let memPos := allocate_unbounded()"
       , " let memEnd := " <> T.pack abienc_builtin <> "(memPos, " <> spread_vars vars <> ")"
       , " hash := keccak256(memPos, sub(memEnd, memPos))"
       , "}"
       ]
     , [ abienc_builtin ])

exports :: [BuiltInEntry]
exports =
  [ cleanup_bool
  , cleanup_uintn
  , cleanup_intn
  , cleanup_address
  , validate_value
  , keccak256
  ]

value_cleanup_builtin_name t = "__cleanup_t_" ++ abiCoreTypeCanonName t

validate_value_builtin_name t = "__validate_t_" ++ abiCoreTypeCanonName t
