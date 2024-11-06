import           Test.Hspec
--
import qualified INTx_prop
import qualified NP_test

main = hspec $ describe "Ethereum.ContractABI types" $ do
  INTx_prop.tests
  NP_test.tests
