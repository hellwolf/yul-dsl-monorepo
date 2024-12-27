import Test.Hspec
--
import INTx_prop qualified
import SimpleNP_test qualified
import TupleN_test qualified

main = hspec $ describe "Ethereum.ContractABI types" $ do
  SimpleNP_test.tests
  TupleN_test.tests
  INTx_prop.tests
