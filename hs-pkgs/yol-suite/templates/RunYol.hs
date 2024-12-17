import           Prelude.Base
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import System.Exit (exitSuccess, exitFailure)
import System.IO (stderr)

import YulDSL.Core
--
-- import qualified YulDSL.CodeGens.PlantUMLGen as PlantUMLCodeGen
import qualified YulDSL.CodeGens.YulGen   as YulCodeGen
import qualified YolSuite.YOLC.Builder as YOLCBuilder

import %YOL_MOD_NAME%

default (String)

type Result = Either T.Text T.Text

data Compiler = MkCompiler
  { fnMode      :: forall eff f. YulO2 (NP (UncurryNP'Fst f)) (UncurryNP'Snd f) => Fn eff f -> IO Result
  , objectMode  :: YulObject -> IO Result
  , projectMode :: YOLCBuilder.Manifest -> IO Result
  }

yulCompiler :: Compiler
yulCompiler = MkCompiler
  { fnMode      = return . Right . YulCodeGen.compileFn . unFn
  , objectMode  = return . Right . YulCodeGen.compileObject
  , projectMode = YOLCBuilder.buildManifest
  }

showCompiler :: Compiler
showCompiler = let f :: Show a => a -> IO Result
                   f = return . Right . T.pack . show
  in MkCompiler f f f

compilers :: [Compiler]
compilers = [yulCompiler, showCompiler]

handleResult :: Result -> IO ()
handleResult (Left err) = TIO.hPutStrLn stderr err >> exitFailure
handleResult (Right out) = TIO.putStrLn out >> exitSuccess

main :: IO ()
main = do
  %COMPILE_ACTIONS%
  return ()
