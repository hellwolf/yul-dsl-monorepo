{-# LANGUAGE NoImplicitPrelude #-}

import           Prelude.Base
import qualified Data.Text.Lazy as T

import YulDSL.Core (YulO2, Fn, YulObject (..))
--
import qualified YulDSL.CodeGen.Yul as YulCodeGen
import qualified YolSuite.YOLC.Builder as YOLCBuilder
-- import qualified YulDSL.CodeGen.PlantUML as PlantUMLCodeGen

import __YOL_MOD_NAME__

default (String)

data Compiler = MkCompiler
  { fnMode      :: forall a b. YulO2 a b => Fn a b ->  IO String
  , objectMode  :: YulObject -> IO String
  , projectMode :: YOLCBuilder.Manifest -> IO String
  }

yulCompiler :: Compiler
yulCompiler = MkCompiler
  { fnMode      = return . T.unpack . YulCodeGen.compileFn
  , objectMode  = return . T.unpack . YulCodeGen.compileObject
  , projectMode = (T.unpack <$>) . YOLCBuilder.buildManifest
  }

showCompiler :: Compiler
showCompiler = MkCompiler (return . show) (return . show) (return . show)

plantumlCompiler :: Compiler
plantumlCompiler = MkCompiler
                   (\_ -> error "Unsupported: fnMode")
                   (\_ -> error "Unsupported: objectMode")
                   (\_ -> error "Unsupported: projectMode")

compilers :: [Compiler]
compilers = [yulCompiler, showCompiler, plantumlCompiler]

main :: IO ()
main = do
  __COMPILE_ACTIONS__
  return ()
