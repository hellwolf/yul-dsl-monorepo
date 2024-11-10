{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ERC20 where

-- | ERC20 balance storage location for the account.
-- TODO should use hashing of course.
erc20_balance_storage :: forall r. YulObj r => ADDR'P r ⊸ ADDR'P r
erc20_balance_storage account = mkUnit account & \(account, unit) -> coerceP $
  coerceP account + yulConst (to_intx @U256 0x42) unit

-- | ERC20 balance of the account.
erc20_balance_of :: YulObj r => ADDR'P r ⊸ U256'P r
erc20_balance_of account = sget (erc20_balance_storage account)

-- | ERC20 transfer function (no negative balance check for simplicity).
erc20_transfer :: Fn (ADDR :* ADDR :* U256) BOOL
erc20_transfer = lfn "transfer" \(from :* to :* amount) ->
    copyAp amount
    (\amount -> passAp from erc20_balance_of & \(from, balance) ->
        erc20_balance_storage from <== balance - amount)
    (\amount -> passAp to erc20_balance_of & \(to, balance) ->
        erc20_balance_storage to <== balance + amount) &
    yulConst true

object = mkYulObject "ERC20" ctor
  [ externalFn erc20_transfer
  ]
  where ctor = YulId
