module YulDSL.Core.YulObject where

import           Data.List                                  (intercalate)
-- eth-abi
import           Ethereum.ContractABI.ABITypeable           (abiTypeCanonName)
import           Ethereum.ContractABI.CoreType.NP
-- import Ethereum.ContractABI.CoreType.NP
import           Ethereum.ContractABI.ExtendedType.SELECTOR (SELECTOR, mkTypedSelector)
--
import           YulDSL.Core.Fn
import           YulDSL.Core.YulCat


-- |  type for the external function call.
data StoragePermission = ReadonlyExternalStorage
                       | WritableExternalStorage
                       | DelegatedStorage

data ScopedFn where
  ExternalFn :: forall eff as b. YulO2 (NP as) b => StoragePermission -> SELECTOR -> FnNP eff as b -> ScopedFn
  LibraryFn  :: forall eff as b. YulO2 (NP as) b => FnNP eff as b -> ScopedFn

externalFn :: forall f as b eff.
              ( YulO2 (NP as) b
              , UncurryNP'Fst f ~ as
              , UncurryNP'Snd f ~ b
              ) => Fn eff f -> ScopedFn
externalFn (MkFn f) = ExternalFn WritableExternalStorage (mkTypedSelector @(NP as) (fnId f)) f

staticFn :: forall f as b eff.
            ( YulO2 (NP as) b
            , UncurryNP'Fst f ~ as
            , UncurryNP'Snd f ~ b
            ) => Fn eff f -> ScopedFn
staticFn (MkFn f) = ExternalFn ReadonlyExternalStorage (mkTypedSelector @(NP as) (fnId f)) f

libraryFn :: forall f as b eff.
             ( YulO2 (NP as) b
             , UncurryNP'Fst f ~ as
             , UncurryNP'Snd f ~ b
             ) => Fn eff f -> ScopedFn
libraryFn (MkFn f) = LibraryFn f

show_fn_spec :: forall as b eff. YulO2 (NP as) b => FnNP eff as b -> String
show_fn_spec f = "fn " <> fnId f <> "(" <> abiTypeCanonName @(NP as) <> ") -> " <> abiTypeCanonName @b

instance Show ScopedFn where
  show (ExternalFn WritableExternalStorage _ f) = "external " <> show_fn_spec f <> "\n" <> show (fnCat f)
  show (ExternalFn ReadonlyExternalStorage _ f) = "static "   <> show_fn_spec f <> "\n" <> show (fnCat f)
  show (ExternalFn DelegatedStorage _ f)        = "delegated "   <> show_fn_spec f <> "\n" <> show (fnCat f)
  show (LibraryFn f)                            = "internal " <> show_fn_spec f <> "\n" <> show (fnCat f)

removeScope :: ScopedFn -> AnyFn
removeScope (ExternalFn _ _ f) = MkAnyFn f
removeScope (LibraryFn f)      = MkAnyFn f

-- | A Yul Object per spec.
--
-- Note:
--   * Do not confuse this with YulObj which is an "object" in the category of YulCat.
--   * Specification: https://docs.soliditylang.org/en/latest/yul.html#specification-of-yul-object
data YulObject = MkYulObject { yulObjectName :: String
                             , yulObjectCtor :: AnyYulCat  -- FIXME support constructor
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
            -> AnyYulCat
            -> [ScopedFn]
            -> YulObject
mkYulObject name ctor afns = MkYulObject { yulObjectName = name
                                         , yulObjectCtor = ctor
                                         , yulObjectSFns = afns
                                         , yulSubObjects = []
                                         }

emptyCtor :: AnyYulCat
emptyCtor = MkAnyYulCat (YulDis @Pure @())
