{-# LANGUAGE ImpredicativeTypes #-}

module YulDSL.Core.YulObject where

import           Data.List               (intercalate)

import           YulDSL.Core.ContractABI
import           YulDSL.Core.YulCat


data Fn a b where
  MkFn :: forall a b. YulO2 a b => { fnId :: String, fnCat :: YulCat a b } -> Fn a b

data AnyFn = forall a b. YulO2 a b => MkAnyFn (Fn a b)

instance YulO2 a b => Show (Fn a b) where show (MkFn _ cat) = show cat

data ScopedFn where
  ExternalFn :: forall a b. YulO2 a b => FuncEffect -> SEL -> Fn a b -> ScopedFn
  LibraryFn  :: forall a b. YulO2 a b => Fn a b -> ScopedFn

externalFn :: forall a b p. YulO2 a b => Fn a b -> ScopedFn
externalFn fn = ExternalFn FuncTx (mkTypedSelector @a @b (fnId fn)) fn

staticFn :: forall a b p. YulO2 a b => Fn a b -> ScopedFn
staticFn fn = ExternalFn FuncStatic (mkTypedSelector @a @b (fnId fn)) fn

libraryFn :: forall a b p. YulO2 a b => Fn a b -> ScopedFn
libraryFn fn = LibraryFn fn

show_fn_spec :: forall a b. YulO2 a b => Fn a b -> String
show_fn_spec fn = "fn " <> fnId fn <> "(" <> abi_type_name @a <> ") -> " <> abi_type_name @b

instance Show ScopedFn where
  show (ExternalFn FuncTx _ fn)     = "external " <> show_fn_spec fn <> "\n" <> show (fnCat fn)
  show (ExternalFn FuncStatic _ fn) = "static "   <> show_fn_spec fn <> "\n" <> show (fnCat fn)
  show (LibraryFn fn)               = "internal " <> show_fn_spec fn <> "\n" <> show (fnCat fn)

removeScope :: ScopedFn -> AnyFn
removeScope (ExternalFn _ _ fn) = MkAnyFn fn
removeScope (LibraryFn fn)      = MkAnyFn fn

-- | A Yul Object per spec.
--
-- Note:
--   * Do not confuse this with YulObj which is an "object" in the category of YulCat.
--   * Specification: https://docs.soliditylang.org/en/latest/yul.html#specification-of-yul-object
data YulObject = MkYulObject { yulObjectName :: String
                             , yulObjectCtor :: YulCat () ()
                             , yulObjectSFns :: [ScopedFn] -- scoped functions
                             , yulSubObjects :: [YulObject]
                             -- , TODO support object data
                             }

instance Show YulObject where
  show o = "-- Functions:\n\n"
           <> intercalate "\n\n" (fmap show (yulObjectSFns  o))
           <> "\n\n-- Init code:\n\n"
           <> (show . yulObjectCtor) o

mkYulObject :: String
            -> YulCat () ()
            -> [ScopedFn]
            -> YulObject
mkYulObject name ctor afns = MkYulObject { yulObjectName = name
                                         , yulObjectCtor = ctor
                                         , yulObjectSFns = afns
                                         , yulSubObjects = []
                                         }
