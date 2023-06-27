import           Test.Hspec

import qualified Eval_prop

main = hspec $ do
  Eval_prop.tests
