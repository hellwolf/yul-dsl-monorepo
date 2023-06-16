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
foo :: YulInternalFunction (One AbiUInt) (One AbiBool)
foo = defun "foo" $ \(x :> ()) ->
  ( copy x & split & \(x1, x2) ->
      -- (+:) is (+) for now, due to not using linear-base as base library yet.
      Addr 0xdeadbeef <=@ x1 +: x2
  ) & yulConst (Bool True :> ())

bar :: YulInternalFunction (AbiUInt :> AbiUInt :> ()) (One AbiBool)
bar = defun "bar" $ \(x1 :> x2 :> ()) ->
  -- (+:) is (+) for now, due to not using linear-base as base library yet.
  (Addr 0xdeadbeef <=@ x1 +: x2) &
  yulConst (Bool True :> ())

-- FIXME should use hashing of course
erc20_balance_storage :: YulObj r => YulP r AbiAddr ⊸ YulP r AbiAddr
erc20_balance_storage account = mkUnit account & \(account, unit) ->
  yulConst (Addr 0x42) unit +: account

-- | ERC20 balance of.
erc20_balance_of :: YulObj r => YulP r AbiAddr ⊸ YulP r AbiUInt
erc20_balance_of account = sget (erc20_balance_storage account)

-- | ERC20 transfer function (no negative balance check for simplicity).
erc20_transfer :: YulInternalFunction (AbiAddr :> AbiAddr :> AbiUInt :> ()) (One AbiBool)
erc20_transfer = defun "transfer" $ \(from :> to :> amount :> ()) ->
  (copyAp amount
    (\amount -> passAp from erc20_balance_of & \(from, balance) ->
        erc20_balance_storage from <== balance -: amount)
    (\amount -> passAp to erc20_balance_of & \(to, balance) ->
        erc20_balance_storage to <== balance +: amount)) &
  yulConst (Bool True :> ())

--  (\err -> ignore (discard err) $ yulConst (yulBool False))

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
  let n = 0
      c = compilers !! n
  putStr $ c "id" (YulId @AbiUInt @AbiUInt)
  putStr $ c "const42" (decode $ yulConst (Int 42) :: YulDSL () AbiInt)
  putStr $ c "foo" foo
  putStr $ c "bar" bar
  putStr $ c "ERC20.transfer" erc20_transfer
