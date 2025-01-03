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

balance_sloc account'p = PureLocation (callFn'lpp erc20_balance_storage_of account'p)
balance_of account'p = sget $ PureLocation (callFn'lpp erc20_balance_storage_of account'p)

-- | ERC20 balance of the account.
erc20_balance_of = lfn $locId $ yulmonad'p @(ADDR -> U256)
  \account'p -> balance_of account'p

erc20_mint = lfn $locId $ yulmonad'p @(ADDR -> U256 -> ())
  \account'p mintAmount'p -> LVM.do
  -- (account'p, balanceBefore) <- pass account'p balance_of
  -- -- update balance
  -- (account'p, mintAmount'p) <- passN_ (account'p, mintAmount'p) $ \(account'p, mintAmount'p) -> LVM.do
  --   mintAmount <- impure mintAmount'p
  --   sput (balance_sloc account'p) (balanceBefore + mintAmount)
  -- -- call external
  -- (account, mintAmount) <- impureN (account'p, mintAmount'p)
  -- externalCall onTokenMinted account mintAmount

  -- NOTE1!! balanceBefore is fetched before calling the callback
  (account'p, balanceBefore) <- pass account'p balance_of
  -- call external
  (account'p, mintAmount'p) <- passN_ (account'p, mintAmount'p) $ \(account'p, mintAmount'p) -> LVM.do
    (account, mintAmount) <- impureN (account'p, mintAmount'p)
    -- NOTE2!! this is a callback to an unsafe external contract
    externalCall onTokenMinted account mintAmount
  -- update balance
  mintAmount <- impure mintAmount'p
  -- NOTE3!! This code will never be able to compile, because data versioning mismatch.
  sput (balance_sloc account'p) (balanceBefore + mintAmount)

erc20_transfer = lfn $locId $ yulmonad'p @(ADDR -> ADDR -> U256 -> BOOL)
  \from'p to'p amount'p -> LVM.do

  -- data generate 0 block: update sender balance
  amount'p <- pass_ amount'p \amount'p -> LVM.do
    (from, amount) <- impureN (from'p, amount'p)
    (from, fromS) <- pass from (pure . VersionedLocation . callFn'l erc20_balance_storage_of)
    sput fromS (callFn'l erc20_balance_of from - amount)

  -- data generation 1 block: update receiver balance
  with amount'p \amount'p -> LVM.do
    (to, amount) <- impureN (to'p, amount'p)
    (to, toS) <- pass to (pure . VersionedLocation . callFn'l erc20_balance_storage_of)
    sput toS (callFn'l erc20_balance_of to + amount)

  embed true

object = mkYulObject "ERC20" emptyCtor
  [ staticFn "balanceOf" erc20_balance_of
  , omniFn   "mint" erc20_mint
  , omniFn   "transfer" erc20_transfer
  ]

-- TODO: to be abstracted in an interface definition
--
onTokenMinted = declareExternalFn @(U256 -> ()) "onTokenMinted"
