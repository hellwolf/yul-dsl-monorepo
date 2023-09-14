{-# LANGUAGE NoImplicitPrelude #-}

import           Prelude.Base
import qualified Data.Text                as T

-- import qualified YulDSL.CodeGen.PlantUML as PlantUMLCodeGen
import qualified YulDSL.CodeGen.Yul as YulCodeGen
import YulDSL.Core (YulO2, Fn, YulObject (..))

import __YOL_MOD_NAME__

default (String)

data Compiler = MkCompiler
  { objectMode :: YulObject -> String
  , fnMode     :: forall a b. YulO2 a b => Fn a b -> String
  }

showCompiler :: Compiler
showCompiler = MkCompiler { objectMode = \o -> "module __YOL_MOD_NAME__ where\n\n" <> show o
                          , fnMode = \fn -> show fn
                          }

plantumlCompiler :: Compiler
plantumlCompiler = MkCompiler
                   (\_ -> "Unsupported")
                   (\_ -> "Unsupported")

yulCompiler :: Compiler
yulCompiler = MkCompiler
              (T.unpack . YulCodeGen.compileObject)
              (T.unpack . YulCodeGen.compileFn)

compilers :: [Compiler]
compilers = [showCompiler, plantumlCompiler, yulCompiler]

main :: IO ()
main = do
  __COMPILE_ACTIONS__
  return ()
