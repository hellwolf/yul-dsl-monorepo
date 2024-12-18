{-# OPTIONS_GHC -Wno-unused-top-binds #-}
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

yul_fn_mode :: forall eff f. YulO2 (NP (UncurryNP'Fst f)) (UncurryNP'Snd f) => Fn eff f -> IO Result
yul_fn_mode = return . Right . YulCodeGen.compileFn . unFn

yul_object_mode :: YulObject -> IO Result
yul_object_mode = return . Right . YulCodeGen.compileObject

yul_project_mode :: YOLCBuilder.Manifest -> IO Result
yul_project_mode = YOLCBuilder.buildManifest

show_fn_mode :: Show a => a -> IO Result
show_fn_mode = return . Right . T.pack . show
show_object_mode :: Show a => a -> IO Result
show_object_mode = show_fn_mode
show_project_mode :: Show a => a -> IO Result
show_project_mode = show_fn_mode

handleResult :: Result -> IO ()
handleResult (Left err) = TIO.hPutStrLn stderr err >> exitFailure
handleResult (Right out) = TIO.putStrLn out >> exitSuccess

main :: IO ()
main = do
  %COMPILE_ACTIONS%
  return ()
