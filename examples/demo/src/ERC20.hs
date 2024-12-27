module ERC20 where
import Control.LinearlyVersionedMonad qualified as LVM
import Prelude.YulDSL


-- | ERC20 balance storage location for the account.
-- TODO should use hashing of course.
erc20_balance_storage account =
  mkUnit account
  & \(account, unit) -> coerce'l (coerce'l account + emb'l (fromInteger @U256 0x42) unit)

-- | ERC20 balance of the account.
erc20_balance_of = fn'l "balanceOf" $ yulmonad'lv @(ADDR -> U256)
  \account -> sget (erc20_balance_storage account)

erc20_mint = fn'l "mint" $ yulmonad'lp @(ADDR -> U256 -> BOOL)
  \account'p amount'p -> LVM.do
  (account, amount) <- impureN (account'p, amount'p)
  sput (erc20_balance_storage account) amount
  embed true

erc20_transfer = fn'l "transfer" $ yulmonad'lp @(ADDR -> ADDR -> U256 -> BOOL)
  \from'p to'p amount'p -> LVM.do

  -- data generate 0 block: update sender balance
  amount'p <- pass_ amount'p \amount'p -> LVM.do
    (amount, from) <- impureN (amount'p, from'p)
    (from, balance1before) <- pass from (pure . call'l erc20_balance_of)
    sput_ (erc20_balance_storage from) (balance1before - amount) -- TODO: operator for storage references

  -- data generation 1 block: update receiver balance
  with amount'p \amount'p -> LVM.do
    (amount, to) <- impureN (amount'p, to'p)
    (to, balance2before) <- pass to (pure . call'l erc20_balance_of)
    sput_ (erc20_balance_storage to) (balance2before + amount)

  embed true

object = mkYulObject "ERC20" emptyCtor
  [ externalFn erc20_balance_of
  , externalFn erc20_mint
  , externalFn erc20_transfer
  ]
