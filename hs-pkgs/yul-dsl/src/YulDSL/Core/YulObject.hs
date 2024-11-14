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


-- | Effect type for the external function call.
data FuncEffect = FuncTx | FuncStatic

data ScopedFn where
  ExternalFn :: forall as b. YulO2 (NP as) b => FuncEffect -> SELECTOR -> FnNP as b -> ScopedFn
  LibraryFn  :: forall as b. YulO2 (NP as) b => FnNP as b -> ScopedFn

externalFn :: forall f as b.
              ( YulO2 (NP as) b
              , UncurryNP'Fst f ~ as
              , UncurryNP'Snd f ~ b
              ) => Fn f -> ScopedFn
externalFn (MkFn f) = ExternalFn FuncTx (mkTypedSelector @(NP as) (fnId f)) f

staticFn :: forall f as b.
            ( YulO2 (NP as) b
            , UncurryNP'Fst f ~ as
            , UncurryNP'Snd f ~ b
            ) => Fn f -> ScopedFn
staticFn (MkFn f) = ExternalFn FuncStatic (mkTypedSelector @(NP as) (fnId f)) f

libraryFn :: forall f as b.
             ( YulO2 (NP as) b
             , UncurryNP'Fst f ~ as
             , UncurryNP'Snd f ~ b
             ) => Fn f -> ScopedFn
libraryFn (MkFn f) = LibraryFn f

show_fn_spec :: forall as b. YulO2 (NP as) b => FnNP as b -> String
show_fn_spec f = "fn " <> fnId f <> "(" <> abiTypeCanonName @(NP as) <> ") -> " <> abiTypeCanonName @b

instance Show ScopedFn where
  show (ExternalFn FuncTx _ f)     = "external " <> show_fn_spec f <> "\n" <> show (fnCat f)
  show (ExternalFn FuncStatic _ f) = "static "   <> show_fn_spec f <> "\n" <> show (fnCat f)
  show (LibraryFn f)               = "internal " <> show_fn_spec f <> "\n" <> show (fnCat f)

removeScope :: ScopedFn -> AnyFn
removeScope (ExternalFn _ _ f) = MkAnyFn f
removeScope (LibraryFn f)      = MkAnyFn f

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
