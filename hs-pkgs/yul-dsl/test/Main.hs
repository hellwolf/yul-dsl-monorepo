import Test.Hspec

import Eval_prop qualified
import Fn_prop qualified

main = hspec $ do
  Fn_prop.tests
  Eval_prop.tests
