{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.BuiltIns (default_builtins) where

-- text
-- import qualified Data.Text.Lazy                               as T
--
import           YulDSL.CodeGens.Yul.Internal.BuiltInRegistra (BuiltInYulGen)


boolean_operators :: [(String, BuiltInYulGen)]
boolean_operators =
  [ ("gt", const "")
  , ("lt", const "")
  , ("eq", const "")
  , ("le", const "function le(a, b) -> r { r := iszero(gt(a, b)) }")
  , ("ge", const "function ge(a, b) -> r { r := iszero(lt(a, b)) }")
  ]

checked_add :: (String, BuiltInYulGen)
checked_add = (,) "__checked_add" $
  const ("function __checked_add_t(a, b) -> r { r := add(a, b) }")

checked_neg :: (String, BuiltInYulGen)
checked_neg = (,) "__checked_neg_t" $
  const ("function __checked_neg_t(a) -> r { r := sub(0, a) }")

maybe_add :: (String, BuiltInYulGen)
maybe_add = (,) "__maybe_add_t" $
  const ("function __maybe_add_t(ta, a, tb, b) -> r { r := add(a, b) }")

default_builtins :: [(String, BuiltInYulGen)]
default_builtins =
  boolean_operators ++
  [ checked_add
  , checked_neg
  , maybe_add
  ]
