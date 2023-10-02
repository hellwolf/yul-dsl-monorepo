import           Test.Hspec

import qualified Eval_prop
import qualified Types_prop

main = hspec $ do
  Types_prop.tests
  Eval_prop.tests
