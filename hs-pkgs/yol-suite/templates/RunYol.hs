{-# LANGUAGE NoImplicitPrelude #-}

import           Prelude.Base
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import System.Exit (exitSuccess, exitFailure)

import YulDSL.Core (YulO2, Fn, YulObject (..))
--
import qualified YulDSL.CodeGen.Yul as YulCodeGen
import qualified YolSuite.YOLC.Builder as YOLCBuilder
-- import qualified YulDSL.CodeGen.PlantUML as PlantUMLCodeGen

import __YOL_MOD_NAME__

default (String)

type Result = Either T.Text T.Text

data Compiler = MkCompiler
  { fnMode      :: forall a b. YulO2 a b => Fn a b ->  IO Result
  , objectMode  :: YulObject -> IO Result
  , projectMode :: YOLCBuilder.Manifest -> IO Result
  }

yulCompiler :: Compiler
yulCompiler = MkCompiler
  { fnMode      = return . Right . YulCodeGen.compileFn
  , objectMode  = return . Right . YulCodeGen.compileObject
  , projectMode = YOLCBuilder.buildManifest
  }

showCompiler :: Compiler
showCompiler = let f :: Show a => a -> IO Result
                   f = return . Right . T.pack . show
  in MkCompiler f f f

plantumlCompiler :: Compiler
plantumlCompiler = MkCompiler
                   (\_ -> error "Unsupported: fnMode")
                   (\_ -> error "Unsupported: objectMode")
                   (\_ -> error "Unsupported: projectMode")

compilers :: [Compiler]
compilers = [yulCompiler, showCompiler, plantumlCompiler]

handleResult :: Result -> IO ()
handleResult (Left err) = TIO.putStrLn err >> exitFailure
handleResult (Right out) = TIO.putStrLn out >> exitSuccess

main :: IO ()
main = do
  __COMPILE_ACTIONS__
  return ()
