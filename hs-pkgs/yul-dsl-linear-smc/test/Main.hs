import           Test.Hspec

import qualified FnL_prop
import qualified Num_prop

main = hspec $ do
  FnL_prop.tests
  Num_prop.tests
