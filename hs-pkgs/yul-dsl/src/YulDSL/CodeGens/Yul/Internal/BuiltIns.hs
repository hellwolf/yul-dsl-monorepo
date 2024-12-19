{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.BuiltIns (default_builtins) where

-- text
import qualified Data.Text.Lazy                               as T
--
import           YulDSL.CodeGens.Yul.Internal.BuiltInRegistra (BuiltInYulGen)


panic_error_0x11 = (,) "panic_error_0x11" $
  const (T.unlines
         [ "function panic_error_0x11() {"
         , "  mstore(0, 35408467139433450592217433187231851964531694900788300625387963629091585785856)"
         , "  mstore(4, 0x11)"
         , "  revert(0, 0x24)"
         , " }"
         ], [])

boolean_operators =
  [ ("gt", const ("", []))
  , ("lt", const ("", []))
  , ("eq", const ("", []))
  , ("le", const ("function le(a, b) -> r { r := iszero(gt(a, b)) }", []))
  , ("ge", const ("function ge(a, b) -> r { r := iszero(lt(a, b)) }", []))
  ]

cleanup_intn = mk_builtin "__cleanup_t_int" $ \t n ->
  let nbytes = read t :: Int
      i = "0x" <> take (nbytes `div` 8 - 1) (repeat 'f')
  in (T.unlines
     [ "function " <> T.pack n <> "(value) -> cleaned {"
     , "  cleaned := signextend(" <> T.pack i <> ", value)"
     , "}"
     ], [])

cleanup_uintn = mk_builtin "__cleanup_t_uint" $ \t n ->
  let nbytes = read t :: Int
      maxVal = "0x" <> take (nbytes `div` 4) (repeat 'f')
  in (T.unlines
      [ "function " <> T.pack n <> "(value) -> cleaned {"
      , "  cleaned := and(value, " <> T.pack maxVal <> ")"
      , "}"
      ], [])

checked_add_uintn = mk_builtin "__checked_add_t_uint" $ \t n ->
  let t' = T.pack t
      nbytes = read t :: Int
      maxVal = "0x" <> take (nbytes `div` 4) (repeat 'f')
  in (T.unlines
       [ "function " <> T.pack n <> "(x, y) -> sum {"
       , "  x := __cleanup_t_uint" <> t' <> "(x)"
       , "  y := __cleanup_t_uint" <> t' <> "(y)"
       , "  sum := add(x, y)"
       , "  if gt(sum, " <> T.pack maxVal <> ") { panic_error_0x11() }"
       , "}"
       ]
     , [ "__cleanup_t_uint" <> t
       , "panic_error_0x11"
       ])

maybe_add_uintn = mk_builtin "__maybe_add_t_uint" $ \t n ->
  let t' = T.pack t
      nbytes = read t :: Int
      maxVal = "0x" <> take (nbytes `div` 4) (repeat 'f')
  in (T.unlines
       [ "function " <> T.pack n <> "(tx, x, ty, y) -> t, sum {"
       , "  t := true"
       , "  x := __cleanup_t_uint" <> t' <> "(x)"
       , "  y := __cleanup_t_uint" <> t' <> "(y)"
       , "  sum := add(x, y)"
       , "  if gt(sum, " <> T.pack maxVal <> ") {"
       , "    t := false"
       , "    sum := 0"
       , "  }"
       , "}"
       ], ["__cleanup_t_uint" <> t])

checked_add_intn = mk_builtin "__checked_add_t_int" $ \t n ->
  let t' = T.pack t
  in (T.unlines
      [ "function " <> T.pack n <> "(x, y) -> sum {"
      , "  x := __cleanup_t_int" <> t' <> "(x)"
      , "  y := __cleanup_t_int" <> t' <> "(y)"
      , "  sum := add(x, y)"
      , "  if or("
      , "    and(iszero(slt(x, 0)), slt(sum, y)),"
      , "    and(slt(x, 0), iszero(slt(sum, y)))"
      , "  ) { panic_error_0x11() }"
      , "}"
      ], ["__cleanup_t_int" <> t])

checked_neg = mk_builtin "__checked_neg_t_uint" $ \_ n ->
  ( "function " <> T.pack n <> "(a) -> r { r := sub(0, a) }"
  , []
  )

allocate_unbounded = (,) "__allocate_unbounded" $
  const ("function __allocate_unbounded() -> memPtr { memPtr := mload(64) }", [])

default_builtins :: [(String, BuiltInYulGen)]
default_builtins =
  boolean_operators ++
  [ panic_error_0x11
  , cleanup_intn
  , cleanup_uintn
  , checked_add_uintn
  , checked_add_intn
  , checked_neg
  , maybe_add_uintn
  , allocate_unbounded
  ]

--
-- Internal function
--

mk_builtin :: String -> (String -> BuiltInYulGen) -> (String, BuiltInYulGen)
mk_builtin prefix g = (prefix, \n -> g (drop (length prefix) n) n)
