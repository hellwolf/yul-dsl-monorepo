{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ERC20 where

-- | ERC20 balance storage location for the account.
-- TODO should use hashing of course.
erc20_balance_storage :: forall r v. YulObj r => ADDR'L v r ⊸ ADDR'L v r
erc20_balance_storage account = mkUnit account & \(account, unit) -> coerce'l $
  coerce'l account + const'l (fromInteger @U256 0x42) unit

-- | ERC20 balance of the account.
erc20_balance_of :: forall r v. YulObj r => ADDR'L v r ⊸ U256'L v r
erc20_balance_of account = sget (erc20_balance_storage account)

-- | ERC20 transfer function (no negative balance check for simplicity).
-- erc20_transfer :: Fn (ADDR -> ADDR -> U256 -> BOOL)
erc20_transfer = fn'l "transfer"
  (curry'l @(ADDR -> ADDR -> U256 -> BOOL) \from to amount ->
      dis'l to &
      \u -> ignore u from &
      \from -> passAp from erc20_balance_of &
      \(from, balance) -> (sput (erc20_balance_storage from) (balance - amount)) &
      -- (\amount -> passAp to erc20_balance_of & \(to, balance) ->
      --     sput (erc20_balance_storage to) (balance + amount)) &
      const'l true
  )

object = mkYulObject "ERC20" emptyCtor
  [ externalFn erc20_transfer
  ]
