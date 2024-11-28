{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ERC20 where

-- | ERC20 balance storage location for the account.
-- TODO should use hashing of course.
-- erc20_balance_storage :: forall r eff. YulObj r => P'xL eff v r ADDR ⊸ P'xL eff v r
erc20_balance_storage account =
  mkUnit account
  & \(account, unit) -> coerce'l (coerce'l account + emb'l (fromInteger @U256 0x42) unit)

-- | ERC20 balance of the account.
erc20_balance_of = fn'l "balanceOf" $ uncurry'l @(ADDR -> U256)
  \account -> sget (erc20_balance_storage account)

-- | ERC20 transfer function (no negative balance check for simplicity).
erc20_transfer = fn'pl "transfer" $ uncurry'pl @(ADDR -> ADDR -> U256 -> BOOL)
  \from to amount -> use'l amount id
  & \(amount, amount') -> use'l (lift'l from) (call'l erc20_balance_of)
  & \(from, balance1before) -> sput (erc20_balance_storage from) (balance1before - (lift'l amount))
  \balance1After -> use'l (ignore (dis'l balance1After) (lift'l to)) (call'l erc20_balance_of)
  & \(to, balance2before) -> sput (erc20_balance_storage to) (balance2before - (lift'l amount'))
  (emb'l true)
  -- & \(amount, amount') -> lift'l from
  -- & \ from -> lift'l to
  -- & \ to -> use'l from (call'l erc20_balance_of)
  -- & \(from, balance1before) -> sput (erc20_balance_storage from) (balance1before - (lift'l amount))
  -- \balance1After -> use'l (ignore (dis'l balance1After) to) (call'l erc20_balance_of)
  -- & \(to, balance2before) -> sput (erc20_balance_storage to) (balance2before - (lift'l amount'))
  -- (emb'l true)

object = mkYulObject "ERC20" emptyCtor
  [ -- externalFn erc20_balance_of FIXME
   externalFn erc20_transfer
  ]
