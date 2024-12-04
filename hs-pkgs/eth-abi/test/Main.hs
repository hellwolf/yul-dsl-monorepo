import           Test.Hspec
--
import qualified INTx_prop
import qualified SimpleNP_test
import qualified TupleN_test

main = hspec $ describe "Ethereum.ContractABI types" $ do
  SimpleNP_test.tests
  TupleN_test.tests
  INTx_prop.tests
