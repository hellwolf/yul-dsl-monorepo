module ERC20 where

import qualified Control.LinearVersionedMonad as LVM
import           Prelude.YulDSL

-- | ERC20 balance storage location for the account.
-- TODO should use hashing of course.
-- erc20_balance_storage :: forall r eff. YulObj r => P'xL eff v r ADDR ⊸ P'xL eff v r
erc20_balance_storage account =
  mkUnit account
  & \(account, unit) -> coerce'l (coerce'l account + emb'l (fromInteger @U256 0x42) unit)

-- | ERC20 balance of the account.
erc20_balance_of = fn'l "balanceOf" $ yulmonad'lv @(ADDR -> U256)
  \account -> sget (erc20_balance_storage account)

erc20_transfer = fn'l "transfer" $ yulmonad'lp @(ADDR -> ADDR -> U256 -> BOOL)
  \from'p to'p amount'p -> LVM.do

  let !(amount'p1, amount'p2) = dup2'l amount'p

  -- state gen 0
  amount1 <- lift'l amount'p1
  from <- lift'l from'p
  let !(from1, from2) = dup2'l from
      balance1before = call'l erc20_balance_of from1
  u1 <- sput_ (erc20_balance_storage from2) (balance1before - amount1)

  -- state gen 1
  to <- lift'l to'p
  amount2 <- lift'l amount'p2
  let !(to1, to2) = dup2'l (ignore u1 to)
      balance1before = call'l erc20_balance_of to1
  u2 <- sput_ (erc20_balance_storage to2) (balance1before - amount2)
  fin'emb true (discard u2)

object = mkYulObject "ERC20" emptyCtor
  [ -- externalFn erc20_balance_of FIXME
   externalFn erc20_transfer
  ]
