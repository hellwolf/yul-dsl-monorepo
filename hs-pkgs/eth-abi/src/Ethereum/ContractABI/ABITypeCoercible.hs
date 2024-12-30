{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : hellwolf@yolc.dev
Stability   : experimental
Portability : GHC2024

-}
module Ethereum.ContractABI.ABITypeCoercible
  ( SameABICoreType, ABITypeCoercible
  ) where

import Ethereum.ContractABI.ABITypeable
import Ethereum.ContractABI.CoreType.NP


type SameABICoreType a a' = ABITypeDerivedOf a ~ ABITypeDerivedOf a'

class ABITypeCoercible a b

-- | Overlappable default instance if @a@ @a'@ are of the same abi core type.
-- instance {-# INCOHERENT #-} forall a a'. (SameABICoreType a a') => ABITypeCoercible a a'
-- instance {-# INCOHERENT #-} forall a a'. a' ~ ABITypeDerivedOf a => ABITypeCoercible a a'

-- unitor coercion instances
instance forall a. ABITypeCoercible a (a, ())
instance forall a. ABITypeCoercible (a, ()) a

-- associative coercion instances
instance forall a b c. ABITypeCoercible ((a, b), c) (a, (b, c))
instance forall a b c. ABITypeCoercible (a, (b, c)) ((a, b), c)

-- NP coercion instances
instance ABITypeCoercible (NP '[]) ()
instance ABITypeCoercible () (NP '[])
instance forall x. ABITypeCoercible x (NP '[x])
instance forall x xs. ABITypeCoercible (x, NP xs) (NP (x:xs))
instance forall x xs. ABITypeCoercible (NP (x:xs)) (x, NP xs)
