module LoliYul.Types where

newtype YulAddr = YulAddr Integer

newtype YulBool = YulBool Bool
yulBool :: Bool -> YulType
yulBool = MkYulBool . YulBool

newtype YulUInt = YulUInt Integer
yulUInt :: Integer -> YulType
yulUInt = MkYulUInt . YulUInt

newtype YulInt  = YulInt Integer
yulInt :: Integer -> YulType
yulInt = MkYulInt . YulInt

data YulType  = MkYulAddr YulAddr
              | MkYulTuple [YulType]
              | MkYulBool YulBool
              | MkYulUInt YulUInt
              | MkYulInt  YulInt

deriving instance Show YulAddr
deriving instance Show YulBool
deriving instance Show YulUInt
deriving instance Show YulInt
deriving instance Show YulType
