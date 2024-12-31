module ERC20 where
import Control.LinearlyVersionedMonad qualified as LVM
import Prelude.YulDSL


-- | ERC20 balance storage location for the account.
erc20_balance_storage_of = fn'l "erc20_balance_storage_of" $
  uncurry'lvv @(ADDR -> B32)
  \acc -> encode yulKeccak256 acc -- FIXME pure function

-- | ERC20 balance of the account.
erc20_balance_of = fn'l "balanceOf" $ yulmonad'v @(ADDR -> U256)
  \account -> LVM.do
    sget (call'l erc20_balance_storage_of account)

erc20_mint = fn'l "mint" $ yulmonad'p @(ADDR -> U256 -> BOOL)
  \account'p amount'p -> LVM.do
  (account, amount) <- impureN (account'p, amount'p)
  sput (call'l erc20_balance_storage_of account) amount
  embed true

erc20_transfer = fn'l "transfer" $ yulmonad'p @(ADDR -> ADDR -> U256 -> BOOL)
  \from'p to'p amount'p -> LVM.do

  -- data generate 0 block: update sender balance
  amount'p <- pass_ amount'p \amount'p -> LVM.do
    (amount, from) <- impureN (amount'p, from'p)
    (from, balance1before) <- pass from (pure . call'l erc20_balance_of)
    sput_ (call'l erc20_balance_storage_of from) (balance1before - amount) -- TODO: operator for storage references

  -- data generation 1 block: update receiver balance
  with amount'p \amount'p -> LVM.do
    (amount, to) <- impureN (amount'p, to'p)
    (to, balance2before) <- pass to (pure . call'l erc20_balance_of)
    sput_ (call'l erc20_balance_storage_of to) (balance2before + amount)

  embed true

object = mkYulObject "ERC20" emptyCtor
  [ staticFn erc20_balance_of
  , externalFn erc20_mint
  , externalFn erc20_transfer
  ]
