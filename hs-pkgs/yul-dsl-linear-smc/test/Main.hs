import Test.Hspec

import LinearFn_prop qualified
import Num_prop qualified

main = hspec do
  Num_prop.tests
  LinearFn_prop.tests
