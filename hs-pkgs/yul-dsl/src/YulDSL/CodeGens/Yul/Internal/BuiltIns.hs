{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.BuiltIns (default_builtins) where

-- text
import qualified Data.Text.Lazy                               as T
--
import           YulDSL.CodeGens.Yul.Internal.BuiltInRegistra (BuiltInEntry, const_builtin, mk_builtin)


------------------------------------------------------------------------------------------------------------------------
-- Arithmetic
------------------------------------------------------------------------------------------------------------------------

raw_boolean_operators =
  [ const_builtin  "eq" ""
  , const_builtin  "gt" ""
  , const_builtin  "lt" ""
  , const_builtin  "le" "function le(a, b) -> r { r := iszero(gt(a, b)) }"
  , const_builtin  "ge" "function ge(a, b) -> r { r := iszero(lt(a, b)) }"
  , const_builtin "sgt" ""
  , const_builtin "slt" ""
  , const_builtin "sle" "function le(a, b) -> r { r := iszero(sgt(a, b)) }"
  , const_builtin "sge" "function ge(a, b) -> r { r := iszero(slt(a, b)) }"
  ]

cleanup_intn = mk_builtin "__cleanup_t_int" $ \part full ->
  let nbits = read part :: Int
      i = "0x" <> take (nbits `div` 8 - 1) (repeat 'f')
  in (T.unlines
     [ "function " <> T.pack full <> "(value) -> cleaned {"
      , if nbits < 256
        then "  cleaned := signextend(" <> T.pack i <> ", value)"
        else "  cleaned := value"
     , "}"
     ], [])

cleanup_uintn = mk_builtin "__cleanup_t_uint" $ \part full ->
  let nbits = read part :: Int
      maxVal = "0x" <> take (nbits `div` 4) (repeat 'f')
  in (T.unlines
      [ "function " <> T.pack full <> "(value) -> cleaned {"
      , if nbits < 256
        then "  cleaned := and(value, " <> T.pack maxVal <> ")"
        else "  cleaned := value"
      , "}"
      ], [])

checked_add_uintn = mk_builtin "__checked_add_t_uint" $ \part full ->
  let part' = T.pack part
      nbits = read part :: Int
      maxVal = "0x" <> take (nbits `div` 4) (repeat 'f')
  in (T.unlines
       [ "function " <> T.pack full <> "(x, y) -> sum {"
       , "  x := __cleanup_t_uint" <> part' <> "(x)"
       , "  y := __cleanup_t_uint" <> part' <> "(y)"
       , "  sum := add(x, y)"
       , "  if " <>
         (if nbits < 256 then "gt(sum, " <> T.pack maxVal <> ")" else "gt(x, sum)")
         <> " { panic_error_0x11() }"
       , "}"
       ]
     , [ "__cleanup_t_uint" <> part
       , "panic_error_0x11"
       ])

maybe_add_uintn = mk_builtin "__maybe_add_t_uint" $ \part full ->
  let part' = T.pack part
      nbits = read part :: Int
      maxVal = "0x" <> take (nbits `div` 4) (repeat 'f') -- TODO use eth-abi
  in (T.unlines
       [ "function " <> T.pack full <> "(tx, x, ty, y) -> t, sum {"
       , "  t := true"
       , "  x := __cleanup_t_uint" <> part' <> "(x)"
       , "  y := __cleanup_t_uint" <> part' <> "(y)"
       , "  sum := add(x, y)"
       , "  if gt(sum, " <> T.pack maxVal <> ") {"
       , "    t := false"
       , "    sum := 0"
       , "  }"
       , "}"
       ]
     , [ "__cleanup_t_uint" <> part
       ])

checked_add_intn = mk_builtin "__checked_add_t_int" $ \part full ->
  let part' = T.pack part
      nbits = read part :: Int
      minVal = "0x7f" <> take (nbits `div` 4 - 2) (repeat 'f')
      maxVal = "0x"   <> take (nbits `div` 4) (repeat 'f') <> "80" <> take (62 - nbits `div` 4) (repeat '0')
  in (T.unlines
       [ "function " <> T.pack full <> "(x, y) -> sum {"
       , "  x := __cleanup_t_int" <> part' <> "(x)"
       , "  y := __cleanup_t_int" <> part' <> "(y)"
       , "  sum := add(x, y)"
       , "  if or("
       , (if nbits < 256
          then "    sgt(sum, " <> T.pack minVal <> "),\n" <>
               "    slt(sum, " <> T.pack maxVal <> ")"
          else "    and(iszero(slt(x, 0)), slt(sum, y)),\n" <>
               "    and(slt(x, 0), iszero(slt(sum, y)))"
         )
       , "  ) { panic_error_0x11() }"
       , "}"
       ]
     , [ "__cleanup_t_int" <> part
       , "panic_error_0x11"
       ])

checked_sub_uintn = mk_builtin "__checked_sub_t_uint" $ \part full ->
  let part' = T.pack part
      nbits = read part :: Int
      maxVal = "0x" <> take (nbits `div` 4) (repeat 'f')
  in (T.unlines
       [ "function " <> T.pack full <> "(x, y) -> diff {"
       , "  x := __cleanup_t_uint" <> part' <> "(x)"
       , "  y := __cleanup_t_uint" <> part' <> "(y)"
       , "  diff := sub(x, y)"
       , "  if gt(diff, " <> T.pack maxVal <> ") { panic_error_0x11() }"
       , "}"
       ]
     , [ "__cleanup_t_uint" <> part
       , "panic_error_0x11"
       ])

checked_sub_intn = mk_builtin "__checked_sub_t_int" $ \part full ->
  let part' = T.pack part
  in (T.unlines
       [ "function " <> T.pack full <> "(x, y) -> diff {"
       , "  x := __cleanup_t_int" <> part' <> "(x)"
       , "  y := __cleanup_t_int" <> part' <> "(y)"
       , "  diff := sub(x, y)"
       , "  if or("
       , "    and(iszero(slt(y, 0)), sgt(diff, x)),"
       , "    and(slt(y, 0), slt(diff, x))"
       , "  ) { panic_error_0x11() }"
       , "}"
       ]
     , [ "__cleanup_t_int" <> part
       ])

cmp_builtins = mk_builtin "__cmp_" $ \part full ->
  let (op, typ) = break (== '_') part
      cleanf = "__cleanup_t" <> typ
  in (T.unlines
       [ "function " <> T.pack full <> "(x, y) -> b {"
       , "  b := " <> T.pack op <> "(" <> T.pack cleanf <> "(x), " <> T.pack cleanf <> "(y))"
       , "}"
       ]
     , [ cleanf
       , op])

arithmetic_builtins =
  raw_boolean_operators ++
  [ cleanup_intn
  , cleanup_uintn
  , checked_add_uintn
  , checked_add_intn
  , maybe_add_uintn
  , checked_sub_uintn
  , checked_sub_intn
  , cmp_builtins
  ]

------------------------------------------------------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------------------------------------------------------

panic_errors = mk_builtin "panic_error_" $ \part full ->
  (T.unlines
    [ "function " <> T.pack full <> "() {"
    -- `cast sig 'Panic(uint256)'` == 0x4e487b71
    , "  mstore(0, 0x4e487b71" <> T.pack (take 56 (repeat '0')) <> ")"
    , "  mstore(4, " <> T.pack part <> ")"
    , "  revert(0, 0x24)"
    , "}"
    ]
  , [])

allocate_unbounded = const_builtin
  "__allocate_unbounded" $
  "function __allocate_unbounded() -> memPtr { memPtr := mload(64) }"

misc_builtins =
  [ panic_errors
  , allocate_unbounded
  ]

------------------------------------------------------------------------------------------------------------------------

default_builtins :: [BuiltInEntry]
default_builtins =
  arithmetic_builtins ++
  misc_builtins
