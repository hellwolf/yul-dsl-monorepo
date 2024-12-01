import           Test.Hspec

import qualified LinearFn_prop
import qualified Num_prop

main = hspec do
  Num_prop.tests
  LinearFn_prop.tests
