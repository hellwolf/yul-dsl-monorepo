{-|

Copyright   : (c) 2023-2024 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : hellwolf@yolc.dev
Stability   : experimental

= Description

Yul object builder. Yul object specification can be found from [solidity
documentation](https://docs.soliditylang.org/en/latest/yul.html#specification-of-yul-object).

-}
module YulDSL.Core.YulObject
  ( StoragePermission (..)
  , ScopedFn (ExternalFn, LibraryFn), withScopedFn
  , pureFn, externalFn, staticFn, libraryFn
  , YulObject (..), mkYulObject
  , emptyCtor
  ) where

-- base
import Data.List                                  (intercalate)
-- eth-abi
import Ethereum.ContractABI.ABITypeable           (abiTypeCanonName)
import Ethereum.ContractABI.CoreType.NP
-- import Ethereum.ContractABI.CoreType.NP
import Ethereum.ContractABI.ExtendedType.SELECTOR (SELECTOR, mkTypedSelector)
--
import YulDSL.Core.YulCat
import YulDSL.Core.YulCatObj
import YulDSL.Effects.Pure


-- |  type for the external function call.
data StoragePermission = NoStorageAccess
                       | ReadOnlyExternalStorage
                       | WritableExternalStorage

data ScopedFn where
  ExternalFn :: forall k { eff :: k } xs b. YulO2 (NP xs) b
             => StoragePermission -> SELECTOR -> NamedYulCat eff (NP xs) b -> ScopedFn
  LibraryFn  :: forall k { eff :: k } xs b. YulO2 (NP xs) b
             => NamedYulCat eff (NP xs) b -> ScopedFn

withScopedFn :: ScopedFn
             -> (forall k { eff :: k } xs b. (YulO2 (NP xs) b ) => NamedYulCat eff (NP xs) b -> a)
             -> a
withScopedFn (ExternalFn _ _ f) g = g f
withScopedFn (LibraryFn f)      g = g f

pureFn :: forall f as b eff.
          ( PureEffect eff
          , YulO2 (NP as) b
          , UncurryNP'Fst f ~ as
          , UncurryNP'Snd f ~ b
          ) => Fn eff f -> ScopedFn
pureFn (MkFn f@(fid, _)) = ExternalFn NoStorageAccess (mkTypedSelector @(NP as) fid) f

staticFn :: forall f as b eff.
            ( StaticEffect eff
            , YulO2 (NP as) b
            , UncurryNP'Fst f ~ as
            , UncurryNP'Snd f ~ b
            ) => Fn eff f -> ScopedFn
staticFn (MkFn f@(fid, _)) = ExternalFn ReadOnlyExternalStorage (mkTypedSelector @(NP as) fid) f

externalFn :: forall f as b eff.
              ( OmniEffect eff
              , YulO2 (NP as) b
              , UncurryNP'Fst f ~ as
              , UncurryNP'Snd f ~ b
              ) => Fn eff f -> ScopedFn
externalFn (MkFn f@(fid, _)) = ExternalFn WritableExternalStorage (mkTypedSelector @(NP as) fid) f

libraryFn :: forall f as b eff.
             ( YulO2 (NP as) b
             , UncurryNP'Fst f ~ as
             , UncurryNP'Snd f ~ b
             ) => Fn eff f -> ScopedFn
libraryFn (MkFn f) = LibraryFn f

instance Show ScopedFn where
  show (ExternalFn NoStorageAccess _ f@(_, cat))         = "pure "  <> show_fn_spec f <> "\n" <> show cat
  show (ExternalFn ReadOnlyExternalStorage _ f@(_, cat)) = "view "    <> show_fn_spec f <> "\n" <> show cat
  show (ExternalFn WritableExternalStorage _ f@(_, cat)) = "external "  <> show_fn_spec f <> "\n" <> show cat
  show (LibraryFn f@(_, cat))                            = "internal "  <> show_fn_spec f <> "\n" <> show cat

show_fn_spec :: forall xs b eff. YulO2 (NP xs) b => NamedYulCat eff (NP xs) b -> String
show_fn_spec (fid, _) = "fn " <> fid <> "(" <> abiTypeCanonName @(NP xs) <> ") -> " <> abiTypeCanonName @b

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
