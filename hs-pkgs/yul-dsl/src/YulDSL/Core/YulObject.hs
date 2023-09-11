module YulDSL.Core.YulObject where

import           Data.List               (intercalate)

import           YulDSL.Core.ContractABI
import           YulDSL.Core.YulCat


data Fn a b = ExternalFn FuncEffect SEL (YulCat a b)
            | LibraryFn String (YulCat a b)

data AnyFn = forall a b. YulO2 a b => MkAnyFn (Fn a b)

externalFn :: forall a b p. YulO2 a b => String -> YulCat a b -> Fn a b
externalFn fname = ExternalFn FuncTx (mkTypedSelector @a @b fname)

staticFn :: forall a b p. YulO2 a b => String -> YulCat a b -> Fn a b
staticFn fname = ExternalFn FuncStatic (mkTypedSelector @a @b fname)

libraryFn :: forall a b p. YulO2 a b => String -> YulCat a b -> Fn a b
libraryFn name = LibraryFn name

removeScope :: Fn a b -> YulCat a b
removeScope (ExternalFn _ _ c) = c
removeScope (LibraryFn _ c)    = c

show_fn_spec :: forall a b. YulO2 a b => String -> YulCat a b -> String
show_fn_spec s _ = "function " <> s <> "(" <> abi_type_name @a <> ") -> " <> abi_type_name @b

instance YulO2 a b => Show (Fn a b) where
  show (ExternalFn FuncTx s c)     = "external " <> show_fn_spec (show s) c
  show (ExternalFn FuncStatic s c) = "static " <> show_fn_spec (show s) c
  show (LibraryFn n c)             = "internal "   <> show_fn_spec n c

instance Show AnyFn where show (MkAnyFn fn) = show fn

-- | A Yul Object per spec.
--
-- Note:
--   * Do not confuse this with YulObj which is an "object" in the category of YulCat.
--   * Specification: https://docs.soliditylang.org/en/latest/yul.html#specification-of-yul-object
data YulObject = MkYulObject { yulObjectName      :: String
                             , yulObjectCtor      :: YulCat () ()
                             , yulObjectFunctions :: [AnyFn]
                             , yulSubObjects      :: [YulObject]
                             -- , TODO support object data
                             }

instance Show YulObject where
  show o = "-- Functions:\n\n"
           <> intercalate "\n\n" (fmap show (yulObjectFunctions  o))
           <> "\n\n-- Init code:\n\n"
           <> (show . yulObjectCtor) o

mkYulObject :: String
            -> YulCat () ()
            -> [AnyFn]
            -> YulObject
mkYulObject name ctor afns = MkYulObject { yulObjectName = name
                                         , yulObjectCtor = ctor
                                         , yulObjectFunctions = afns
                                         , yulSubObjects = []
                                         }
