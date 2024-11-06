{-# LANGUAGE TypeFamilies #-}

{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : GHC2024

= Description

Basic operations on type-level lists.

-}
module Internal.Data.Type.List
  ( Head, Tail
  , type (++)
  ) where

import           Data.Kind    (Type)
import           GHC.TypeLits (ErrorMessage (Text), TypeError)


type family Head (xs :: [Type]) :: Type where
  Head (a:_) = a
  Head '[] = TypeError (Text "NPHead '[] = _|_")

type family Tail (xs :: [Type]) :: [Type] where
  Tail (_:as) = as
  Tail '[] = '[]

type family (++) (xs :: [Type]) (ys :: [Type]) :: [Type] where
  '[] ++ ys = ys
  (x:xs) ++ ys = x : xs ++ ys
