{-# LANGUAGE UndecidableInstances #-}
{-|

Copyright   : (c) 2024 Miao, ZhiCheng
License     : MIT

Maintainer  : hellwolf@yolc.dev
Stability   : experimental
Portability : GHC2024

= Description

Basic operations on type-level lists.

-}
module Internal.Data.Type.List
  ( Length, Head, Tail, type (++)
  ) where

import GHC.List     (List)
import GHC.TypeLits (ErrorMessage (Text), Nat, TypeError, type (+))


type Length :: List k -> Nat
type family Length xs where
  Length '[]    = 0
  Length (_:xs) = 1 + Length xs

type Head :: List k -> k
type family Head xs where
  Head (a:_) = a
  Head '[] = TypeError (Text "NPHead '[] = _|_")

type Tail :: List k -> List k
type family Tail xs where
  Tail (_:as) = as
  Tail '[] = '[]

type (++) :: List k -> List k -> List k
type family (++) xs ys where
  '[] ++ ys = ys
  (x:xs) ++ ys = x : xs ++ ys
