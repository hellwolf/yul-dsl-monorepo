{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE IncoherentInstances #-}

{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

Safe yul data coercion support for same representational data.

It is the code separated out in order to isolate the usage of UndecidableInstances extension.

-}

module YulDSL.Core.ContractABI.Coerce
  ( YulCoercible
  ) where

import           YulDSL.Core.ContractABI.Types

-- | Family of objects that have the same bytes representations.
class (ABIType a, ABIType b) => YulCoercible a b

instance ABIType a => YulCoercible a a

-- Product type
--

instance ABIType a => YulCoercible a (a, ())
instance ABIType a => YulCoercible (a, ()) a
instance ABIType a => YulCoercible a ((), a)
instance ABIType a => YulCoercible ((), a) a
instance (ABIType a, ABIType b, ABIType c) => YulCoercible (a, (b, c)) ((a, b), c)
instance (ABIType a, ABIType b, ABIType c) => YulCoercible ((a, b), c) (a, (b, c))
instance (ABIType a, ABIType as)  => YulCoercible (a, as) (a :* as)
instance (ABIType a, ABIType as)  => YulCoercible (a :* as) (a, as)

-- Equivalent value types for derivative types
--

instance YulCoercible UINT256 ADDR
instance YulCoercible ADDR UINT256
