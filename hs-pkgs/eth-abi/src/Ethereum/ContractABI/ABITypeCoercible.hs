{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : GHC2024

-}
module Ethereum.ContractABI.ABITypeCoercible
  ( ABITypeCoercible (..)
  ) where

import           Ethereum.ContractABI.ABITypeable
import           Ethereum.ContractABI.CoreType.NP


type CoercibleABITypes a a' = ABITypeDerivedOf a ~ ABITypeDerivedOf a'

-- | Evidences for coercible abi types which have the same run-time representation.
data ABITypeCoercible a b where
  -- ^ ABI types having the same ABITypeDerivedOf are coercible with each other.
  CoercibleABITypes :: forall a a'.
                       ( CoercibleABITypes a a'
                       ) => ABITypeCoercible a a'
  -- ^ Right unitor coercion rule.
  CoercibleUnitor  :: forall a a'.
                      ( CoercibleABITypes a a'
                      ) => ABITypeCoercible a (a', ())
  -- ^ Left unitor coercion rule.
  CoercibleUnitor' :: forall a a'.
                      ( CoercibleABITypes a a'
                      ) => ABITypeCoercible (a, ()) a'
  -- ^ Right associative coercion rule.
  CoercibleAssoc  :: forall a b c a' b' c'.
                     ( CoercibleABITypes a a'
                     , CoercibleABITypes b b'
                     , CoercibleABITypes c c'
                     ) => ABITypeCoercible ((a, b), c) (a', (b', c'))
  -- ^ Left associative coercion rule.
  CoercibleAssoc' :: forall a b c a' b' c'.
                     ( CoercibleABITypes a a'
                     , CoercibleABITypes b b'
                     , CoercibleABITypes c c'
                     ) => ABITypeCoercible (a, (b, c)) ((a', b'), c')
  -- ^ Any ABI type is coercible to its solo tuple.
  CoercibleSolo :: forall a a'.
                   ( CoercibleABITypes a a'
                   ) => ABITypeCoercible a (NP '[a'])
  -- ^ A tuple of
  CoercibleTupleAndNP :: forall a a' as.
                         ( CoercibleABITypes a a'
                         ) => ABITypeCoercible (a, NP as) (NP (a':as))
