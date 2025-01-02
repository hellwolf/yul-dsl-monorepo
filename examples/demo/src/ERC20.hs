module ERC20 where
import Control.LinearlyVersionedMonad qualified as LVM
import Prelude.YulDSL


-- | ERC20 balance storage location for the account.
--
-- TODO: this code can be made more palatable in the future versions of Yolc.
erc20_balance_storage_of = fn @(ADDR -> B32) $locId $
  \acc -> yulKeccak256 $
          -- shell$ cast keccak "Yolc.Demo.ERC20.Storage.AccountBalance"
          (YulEmb (0xc455e3e95e9cd89a306d7619bc5f6406a85b850d31788d0c0fb15e6364be6592 :: U256))
          `YulFork` acc

-- | ERC20 balance of the account.
erc20_balance_of = lfn $locId $ yulmonad'p @(ADDR -> U256)
  \account'p -> LVM.do
  accountS <- impure (callFn'lpp erc20_balance_storage_of account'p)
  sget accountS

erc20_mint = lfn $locId $ yulmonad'p @(ADDR -> U256 -> BOOL)
  \account'p amount'p -> LVM.do
  (to, amount) <- impureN (callFn'lpp erc20_balance_storage_of account'p, amount'p)
  sput to amount
  embed true

erc20_transfer = lfn $locId $ yulmonad'p @(ADDR -> ADDR -> U256 -> BOOL)
  \from'p to'p amount'p -> LVM.do

  -- data generate 0 block: update sender balance
  amount'p <- pass_ amount'p \amount'p -> LVM.do
    (from, amount) <- impureN (from'p, amount'p)
    (from, fromS) <- pass from (pure . callFn'l erc20_balance_storage_of)
    sput_ fromS (callFn'l erc20_balance_of from - amount) -- TODO: operator for storage references

  -- data generation 1 block: update receiver balance
  with amount'p \amount'p -> LVM.do
    (to, amount) <- impureN (to'p, amount'p)
    (to, toS) <- pass to (pure . callFn'l erc20_balance_storage_of)
    sput_ toS (callFn'l erc20_balance_of to + amount)

  embed true

object = mkYulObject "ERC20" emptyCtor
  [ staticFn "balanceOf" erc20_balance_of
  , omniFn   "mint" erc20_mint
  , omniFn   "transfer" erc20_transfer
  ]
