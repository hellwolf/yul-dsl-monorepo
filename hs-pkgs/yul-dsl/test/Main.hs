import           Test.Hspec

import qualified Eval_prop
import qualified Fn_prop

main = hspec $ do
  Fn_prop.tests
  Eval_prop.tests
