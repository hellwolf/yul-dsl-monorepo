{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import           Control.Category.Linear
import qualified Data.Text                as T

import qualified LoliYul.CodeGen.PlantUML
import           LoliYul.Core

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
erc20_balance_of :: YulCon r => YulPort r YulAddr ⊸ YulPort r YulUInt
erc20_balance_of account = sget (erc20_balance_storage account)

-- | ERC20 transfer function (no negative balance check for simplicity).
erc20_transfer :: YulFunction
erc20_transfer = defun "transfer" $ \p ->
  -- A bit of boilerplate for input abi decoding parsing,
  -- a more sugarized version of abiDecode could do better.
  -- Also need error handling to make it a complete function instead of revert as exception.
  (abiIter @YulAddr p & \(from, p) -> abiIter @YulAddr p & \(to, p) -> abiPop @YulUInt p & \amount ->
      copyAp amount
      (\amount -> copyAp' from id1 erc20_balance_of & \(from, balance) ->
          erc20_balance_storage from <== balance -: amount)
      (\amount -> copyAp' to id1 erc20_balance_of & \(to, balance) ->
          erc20_balance_storage to <== balance +: amount)
  ) & abiReturn [yulBool True]


--------------------------------------------------------------------------------
-- Test Run Interface
--------------------------------------------------------------------------------

compilers = [ \name cat -> "# " <> name <> ":\n\n" ++
                           show cat <> "\n" ++
                           replicate 80 '-' <> "\n"
            , \name cat -> T.unpack (LoliYul.CodeGen.PlantUML.compile name cat) ++
                           replicate 80 '-' <> "\n"
            ]

main = do
  let n = 1
      c = compilers !! n
  putStr $ c "id" YulIdentity
  putStr $ c "const42" (decode $ yulConst (yulInt 42) :: YulCat () YulType)
  putStr $ c "foo" foo
  putStr $ c "ERC20.transfer" erc20_transfer
