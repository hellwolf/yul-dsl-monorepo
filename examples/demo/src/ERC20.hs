module ERC20 where

import Prelude.YulDSL
import qualified Control.LinearVersionedMonad as LVM

-- | ERC20 balance storage location for the account.
-- TODO should use hashing of course.
-- erc20_balance_storage :: forall r eff. YulObj r => P'xL eff v r ADDR ⊸ P'xL eff v r
erc20_balance_storage account =
  mkUnit account
  & \(account, unit) -> coerce'l (coerce'l account + emb'l (fromInteger @U256 0x42) unit)

-- | ERC20 balance of the account.
erc20_balance_of = fn'l "balanceOf" $ uncurry'l @(ADDR -> U256)
  \account -> runYulMonad $
              sget (erc20_balance_storage account)

-- | ERC20 transfer function (no negative balance check for simplicity).
-- erc20_transfer = fn'pl "transfer" $ uncurry'pl @(ADDR -> ADDR -> U256 -> BOOL)
--   \from'p to'p amount'p -> startLTM $
--   lift'l to'p
--   &+ dis'l
--   -- &- discard amount'p2
--   -- &- dup2'l amount'p
--   -- &  \(amount'p1, amount'p2) -> lift'l amount'p1
--   &- lift'l amount'p
--   &+ \amount -> lift'l from'p
--   &+ \from -> use'l from (call'l erc20_balance_of)
--   &+ \from balance1before -> sput (erc20_balance_storage from) (balance1before - amount)
--   -- &- lift'l to'p
--   -- &+ \to -> lift'l amount'p2
--   -- &+ \amount -> use'l to (call'l erc20_balance_of)
--   -- &+ \to balance2before -> sput (erc20_balance_storage to) (balance2before - amount)
--   &+ fin'emb true

erc20_transfer = fn'pl "transfer" $ uncurry'pl @(ADDR -> ADDR -> U256 -> BOOL)
  \from'p to'p amount'p -> runYulMonad $ LVM.do

  let %1 !(amount'p1, amount'p2) = dup2'l amount'p

  -- state gen 0
  amount1 <- lift'l amount'p1
  from <- lift'l from'p
  let %1 !(from1, from2) = dup2'l from
      %1 balance1before = call'l erc20_balance_of from1
  u1 <- sput_ (erc20_balance_storage from2) (balance1before - amount1)

  -- state gen 1
  to <- lift'l to'p
  amount2 <- lift'l amount'p2
  let %1 !(to1, to2) = dup2'l (ignore u1 to)
      %1 balance1before = call'l erc20_balance_of to1
  u2 <- sput_ (erc20_balance_storage to2) (balance1before - amount2)
  fin'emb true (discard u2)

object = mkYulObject "ERC20" emptyCtor
  [ -- externalFn erc20_balance_of FIXME
   externalFn erc20_transfer
  ]
