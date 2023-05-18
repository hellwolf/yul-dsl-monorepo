{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import           Control.Category.Linear
import           Data.Proxy              (Proxy (..))

import           LoliYul

--------------------------------------------------------------------------------
-- Contract Examples
--------------------------------------------------------------------------------

-- | A function that takes a UInt input and store its value doubled at a fixed storage location.
foo :: YulFunction
foo = defun "foo" $ \p ->
  (abiPop @YulUInt p & \x ->
      copy x & split & \(x1, x2) ->
      -- (+:) is (+) for now, due to not using linear-base as base library yet.
      YulAddr 0xdeadbeef <=@ x1 +: x2
  ) & abiReturn [yulBool True]

-- FIXME should use hashing of course
erc20_balance_storage :: YulCon r => YulPort r YulAddr ⊸ YulPort r YulAddr
erc20_balance_storage account = yulConst (YulAddr 0x42) unit +: account

  -- | ERC20 balance of.
erc20_balance_of :: YulCon r => YulPort r YulAddr ⊸ YulPort r (YulAddr⊗YulUInt)
erc20_balance_of account = copy account & split & \(a1, a2) -> merge (a1, sget (erc20_balance_storage a2))

  -- | ERC20 transfer function (no negative balance check for simplicity).
erc20_transfer :: YulFunction
erc20_transfer = defun "transfer" $ \p ->
  -- A bit of boilerplate for input abi decoding parsing,
  -- a more sugarized version of abiDecode could do better.
  -- Also need error handling to make it a complete function instead of revert as exception.
  (abiIter @YulAddr p & \(from, p) -> abiIter @YulAddr p & \(to, p) -> abiPop @YulUInt p & \amount ->
      copy amount & split & \(amount1, amount2) -> unit &
      ignore ( erc20_balance_of from & split & \(from, balance) ->
                 erc20_balance_storage from <== balance -: amount1) &
      ignore ( erc20_balance_of to & split & \(to, balance) ->
                 erc20_balance_storage to <== balance +: amount2)
  ) & abiReturn [yulBool True]

main = do
  putStrLn $ "yulConst ():\n" <> show (decode $ yulConst () :: YulCat () ()) <> "\n"
  putStrLn $ "yulConst 42:\n" <> show (decode $ yulConst (yulInt 42) :: YulCat () YulType) <> "\n"
  putStrLn $ "foo:\n" <> show foo <> "\n"
  putStrLn $ "foo:\n" <> show erc20_transfer <> "\n"
