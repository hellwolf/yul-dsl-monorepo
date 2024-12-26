-- base
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
-- text
import qualified Data.Text.Lazy.IO as TIO
-- yol-suite
import YolSuite.YOLC.RunYolModes
--
import %YOL_MOD_NAME%

main :: IO ()
main = %COMPILE_ACTIONS% >>= \case
  Left  err -> TIO.hPutStrLn stderr err >> exitFailure
  Right out -> TIO.putStrLn out >> exitSuccess
