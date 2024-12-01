module ERC20 where

import qualified Control.LinearlyVersionedMonad as LVM
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

  amount1 <- lift amount'p1 -- TODO: lift N variables a time
  from <- lift from'p
  (from, balance1before) <- pass from (pure . call'l erc20_balance_of)
  sput_ (erc20_balance_storage from) (balance1before - amount1) -- TODO: operator for storage references

  to <- lift to'p
  amount2 <- lift amount'p2
  (to, balance2before) <- pass to (pure . call'l erc20_balance_of)
  u2 <- sput_ (erc20_balance_storage to) (balance2before - amount2)

  pure $ emb'l true u2 -- the following code bugs out...
  -- embed true

object = mkYulObject "ERC20" emptyCtor
  [ -- externalFn erc20_balance_of FIXME
   externalFn erc20_transfer
  ]
