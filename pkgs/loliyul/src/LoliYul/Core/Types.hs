module LoliYul.Core.Types where

import           Data.ByteString (ByteString)

-- Primitive Types

newtype AbiAddr  = Addr Integer
newtype AbiBool  = Bool Bool
newtype AbiUInt  = UInt Integer
newtype AbiInt   = Int  Integer
newtype AbiBytes = Blob ByteString

deriving instance Show AbiAddr
deriving instance Show AbiBool
deriving instance Show AbiUInt
deriving instance Show AbiInt
deriving instance Show AbiBytes

-- Heterogeneous List

-- | Type constructor (:>) and data constructor pun for creating heterogeneous list.
data a :> b = a :> b
-- | Operator (:>) being right associative allows bracket-free syntax.
infixr :>

-- | Zero-element type list.
type Nil = () :> ()
-- | Single-element type list.
type One a = a :> ()

instance {-# OVERLAPPABLE #-} (Show a, Show b) => Show (a :> b) where
  show (a :> b) = "(" <> show a <> "," <> show b <> ")"
instance {-# OVERLAPPING  #-} Show a => Show (a :> ()) where
  show (a :> ()) = show a
