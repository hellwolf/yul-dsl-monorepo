{-# LANGUAGE NoImplicitPrelude #-}

import           Prelude.Base
import qualified Data.Text                as T

-- import qualified YulDSL.CodeGen.PlantUML as PlantUMLCodeGen
import qualified YulDSL.CodeGen.Yul as YulCodeGen
import YulDSL.Core (YulO2, Fn, YulObject (..))
import YOLC.Manifest (Manifest (..))

import __YOL_MOD_NAME__

default (String)

data Compiler = MkCompiler
  { fnMode      :: forall a b. YulO2 a b => Fn a b -> String
  , objectMode  :: YulObject -> String
  , projectMode :: Manifest -> String
  }

showCompiler :: Compiler
showCompiler = MkCompiler { fnMode = \fn -> show fn
                          , objectMode = \o -> "module __YOL_MOD_NAME__ where\n\n" <> show o
                          , projectMode = \_ -> "..."
                          }

plantumlCompiler :: Compiler
plantumlCompiler = MkCompiler
                   (\_ -> "Unsupported: fnMode")
                   (\_ -> "Unsupported: objectMode")
                   (\_ -> "Unsupported: projectMode")

yulCompiler :: Compiler
yulCompiler = MkCompiler
  { fnMode = T.unpack . YulCodeGen.compileFn
  , objectMode = T.unpack . YulCodeGen.compileObject
  , projectMode = \_ -> "Unsupported: projectMode"
  }

compilers :: [Compiler]
compilers = [showCompiler, plantumlCompiler, yulCompiler]

main :: IO ()
main = do
  __COMPILE_ACTIONS__
  return ()
