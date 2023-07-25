{-# LANGUAGE NoImplicitPrelude #-}

import           BasePrelude
import qualified Data.Text                as T

import qualified YulDSL.CodeGen.PlantUML
import YulDSL.Core (YulO2, Fn (..), YulObject (..))

import __YOL_MOD_NAME__

type FnCompiler = forall a b.  YulO2 a b => Fn a b -> String
type ObjectCompiler = YulObject -> String

default (String)

compilers :: [(ObjectCompiler, FnCompiler)]
compilers = [
  -- Show mode
  ( \o -> "module __YOL_MOD_NAME__ where\n\n" <> show o
  , \fn -> "module __YOL_MOD_NAME__ where\n\n" <> show fn
  ),
  -- PlantUML mode
  ( \_ -> "Unsupported"
  , \(MkFn name cat) -> T.unpack (YulDSL.CodeGen.PlantUML.compile name cat) ++
                        "' " <> replicate 98 '-' <> "\n"
  )]

main :: IO ()
main = do
  __COMPILE_SYMBOLS__
  return ()
