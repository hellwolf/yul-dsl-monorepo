{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

Copyright   : (c) 2023 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental

= Description

Safe yul data coercion support for same representational data.

It is the code separated out in order to isolate the usage of UndecidableInstances extension.

-}

module LoliYul.Core.YulDSL.Coerce
  ( YulCoercible
  ) where

import           Control.Category.Constrained (type (⊗))

import           LoliYul.Core.ContractABI
import           LoliYul.Core.YulDSL.Obj

-- | Family of objects that have the same bytes representations.
class YulO2 a b => YulCoercible a b

instance {-# OVERLAPPABLE #-} YulCoercible b a => YulCoercible a b

instance {-# OVERLAPPING #-} YulCoercible UINT256 ADDR

instance {-# OVERLAPPING #-} YulO1 a     => YulCoercible (a⊗()) a
instance {-# OVERLAPPING #-} YulO2 a as  => YulCoercible (a :> as) (a⊗as)
instance {-# OVERLAPPING #-} YulO3 a b c => YulCoercible (a⊗(b⊗c)) ((a⊗b)⊗c)
