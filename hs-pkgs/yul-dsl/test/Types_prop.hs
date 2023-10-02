module Types_prop where

import           Test.Hspec
-- import           Test.QuickCheck

import           YulDSL.Core.ContractABI.Types


sig_tbl = [ (mkTypedSelector @(ADDR :* ADDR :* UINT256) "transfer", 0xbeabacc8)
          , (mkTypedSelector @ADDR "balanceOf", 0x70a08231)
          , (mkTypedSelector @(UINT64 :* ADDR) "passphrase_calculate_transfer", 0x70a08231)
          ]
test_sig_tbl = all t sig_tbl where
  t (SEL (sig, _), expectedSig) = sig == expectedSig

tests = describe "YulDSL.Core.ContractABI.Types" $ do
  describe "mkTypedSelector" $ do
    it "Well-known function selectors" test_sig_tbl
