{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Basic where

import           Data.List                ((!!))
import qualified Data.Text                as T

import qualified LoliYul.CodeGen.PlantUML
import           LoliYul.Core

------------------------------------------------------------------------------------------------------------------------
-- Trivial Diagrams
------------------------------------------------------------------------------------------------------------------------

simple_id = YulId @UINT256
simple_coerce = YulCoerce @UINT256 @INT256
const_42 = decode (yulConst (to_intx 42)) :: YulDSL () UINT256

------------------------------------------------------------------------------------------------------------------------
-- Yul Internal Functions
------------------------------------------------------------------------------------------------------------------------

-- | A function that takes a UInt input and store its value doubled at a fixed storage location.
foo :: Fn (One UINT256) (One BOOL)
foo = defun "foo" \(x :> ()) ->
  (copy x & split & \(x1, x2) ->
      to_addr 0xdeadbeef <=@ x1 + x2
  ) &
  yulConst (true :> ())

foo2 :: Fn (UINT256 :> UINT256 :> ()) (One BOOL)
foo2 = defun "foo" \(x1 :> x2 :> ()) ->
  (to_addr 0xdeadbeef <=@ x1 + x2) &
  yulConst (true :> ())

------------------------------------------------------------------------------------------------------------------------
-- ERC20 Copycat
------------------------------------------------------------------------------------------------------------------------

-- | ERC20 balance storage location for the account.
-- TODO should use hashing of course.
erc20_balance_storage :: forall r. YulObj r => AddrP r ⊸ AddrP r
erc20_balance_storage account = mkUnit account & \(account, unit) -> yulCoerce $
  yulCoerce account + yulConst (to_uint256 0x42) unit

-- | ERC20 balance of the account.
erc20_balance_of :: YulObj r => AddrP r ⊸ Uint256P r
erc20_balance_of account = sget (erc20_balance_storage account)

-- | ERC20 transfer function (no negative balance check for simplicity).
erc20_transfer :: Fn (ADDR :> ADDR :> UINT256 :> ()) (One BOOL)
erc20_transfer = defun "transfer" \(from :> to :> amount :> ()) ->
  (copyAp amount
    (\amount -> passAp from erc20_balance_of & \(from, balance) ->
        erc20_balance_storage from <== balance - amount)
    (\amount -> passAp to erc20_balance_of & \(to, balance) ->
        erc20_balance_storage to <== balance + amount)) &
  yulConst (true :> ())

--------------------------------------------------------------------------------
-- Test Functions
--------------------------------------------------------------------------------

compilers = [ \name cat -> "# " <> name <> ":\n\n" ++
                           show cat <> "\n" ++
                           replicate 80 '-' <> "\n"
            , \name cat -> T.unpack (LoliYul.CodeGen.PlantUML.compile name cat) ++
                           replicate 80 '-' <> "\n"
            ]

print_all = do
  let n = 0
      c = compilers !! n
  putStr $ c "simple_id" simple_id
  putStr $ c "simple_coerce" simple_coerce
  putStr $ c "const_42" const_42
  putStr $ c "foo" foo
  putStr $ c "foo2" foo2
  putStr $ c "ERC20.transfer" erc20_transfer
