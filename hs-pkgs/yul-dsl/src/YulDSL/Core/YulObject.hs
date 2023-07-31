module YulDSL.Core.YulObject where

import           Data.List               (intercalate)

import           YulDSL.Core.ContractABI
import           YulDSL.Core.YulCat


data ScopedFn = ExternalFn Selector AnyFn | StaticFn Selector AnyFn | InternalFn AnyFn

externalFn :: forall a b. YulO2 a b => Fn a b -> ScopedFn
externalFn = (ExternalFn 0). MkAnyFn

staticFn :: forall a b. YulO2 a b => Fn a b -> ScopedFn
staticFn = (StaticFn 0) . MkAnyFn

internalFn :: forall a b. YulO2 a b => Fn a b -> ScopedFn
internalFn = InternalFn . MkAnyFn

removeScope :: ScopedFn -> AnyFn
removeScope (ExternalFn _ f) = f
removeScope (StaticFn   _ f) = f
removeScope (InternalFn f)   = f

instance Show ScopedFn where
  show (ExternalFn _ f) = "external " <> show f
  show (StaticFn _ f)   = "static "   <> show f
  show (InternalFn f)   = "internal " <> show f

-- | A Yul Object per spec.
--
-- Note:
--   * Do not confuse this with YulObj which is an "object" in the category of YulCat.
--   * Specification: https://docs.soliditylang.org/en/latest/yul.html#specification-of-yul-object
data YulObject = MkYulObject { yulObjectName      :: String
                             , yulObjectCtor      :: YulCat () ()
                             , yulObjectFunctions :: [ScopedFn]
                             , yulSubObjects      :: [YulObject]
                             -- , TODO support object data
                             }

instance Show YulObject where
  show o = "-- Functions:\n\n"
           <> intercalate "\n\n" (fmap show (yulObjectFunctions  o))
           <> "\n\n-- Init code:\n\n"
           <> (show . yulObjectCtor) o

mkYulObject :: String -> YulCat () () -> [ScopedFn] -> YulObject
mkYulObject name ctor fns = MkYulObject { yulObjectName = name
                                        , yulObjectCtor = ctor
                                        , yulObjectFunctions = fns
                                        , yulSubObjects = []
                                        }
