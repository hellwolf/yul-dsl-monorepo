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
  ( FnEffect (PureFnEffect, StaticFnEffect, OmniFnEffect)
  , ExportedFn (MkExportedFn), withExportedFn
  , pureFn,staticFn, omniFn
  , YulObject (..), mkYulObject
  , emptyCtor
  ) where

-- base
import Data.List                                  (intercalate)
-- eth-abi
import Ethereum.ContractABI.ABITypeable           (abiTypeCanonName)
import Ethereum.ContractABI.CoreType.NP
-- import Ethereum.ContractABI.CoreType.NP
import Ethereum.ContractABI.ExtendedType.SELECTOR
--
import YulDSL.Core.YulCat
import YulDSL.Core.YulCatObj
import YulDSL.Effects.Pure


-- | Type for the exported yul function.
data FnEffect = PureFnEffect
              | StaticFnEffect
              | OmniFnEffect

-- | Exported yul function.
data ExportedFn where
  MkExportedFn :: forall k { eff :: k } xs b. YulO2 (NP xs) b
               => SELECTOR -> FnEffect -> NamedYulCat eff (NP xs) b -> ExportedFn

withExportedFn :: ExportedFn
  -> (forall k { eff :: k } xs b. (YulO2 (NP xs) b ) => NamedYulCat eff (NP xs) b -> a)
  -> a
withExportedFn (MkExportedFn _ _ f) g = g f

pureFn :: forall f as b eff.
  ( PureEffect eff
  , YulO2 (NP as) b
  , UncurryNP'Fst f ~ as
  , UncurryNP'Snd f ~ b
  ) => String -> Fn eff f -> ExportedFn
pureFn fname (MkFn f) = MkExportedFn (mkTypedSelector @(NP as) fname) PureFnEffect f

staticFn :: forall f as b eff.
  ( StaticEffect eff
  , YulO2 (NP as) b
  , UncurryNP'Fst f ~ as
  , UncurryNP'Snd f ~ b
  ) => String -> Fn eff f -> ExportedFn
staticFn fname (MkFn f) = MkExportedFn (mkTypedSelector @(NP as) fname) StaticFnEffect f

omniFn :: forall f as b eff.
  ( OmniEffect eff
  , YulO2 (NP as) b
  , UncurryNP'Fst f ~ as
  , UncurryNP'Snd f ~ b
  ) => String -> Fn eff f -> ExportedFn
omniFn fname (MkFn f) = MkExportedFn (mkTypedSelector @(NP as) fname) OmniFnEffect f

instance Show ExportedFn where
  show (MkExportedFn s PureFnEffect   cat) = "pure "   <> show_fn_spec s cat
  show (MkExportedFn s StaticFnEffect cat) = "static " <> show_fn_spec s cat
  show (MkExportedFn s OmniFnEffect   cat) = "omni "   <> show_fn_spec s cat

show_fn_spec :: forall xs b eff. YulO2 (NP xs) b => SELECTOR -> NamedYulCat eff (NP xs) b -> String
show_fn_spec (SELECTOR (sig, fsig)) cat@(cid, _) =
  let fspec = case fsig of
                Just (FuncSig (fname, _)) -> fname ++ "," ++ show sig ++ "," ++ cid
                Nothing                   -> show sig ++ "," ++ cid
  in "fn " <> fspec <> "(" <> abiTypeCanonName @(NP xs) <> ") -> " <> abiTypeCanonName @b <> "\n" <>
     show cat

-- | A Yul Object per spec.
--
-- Note:
--   * Do not confuse this with YulObj which is an "object" in the category of YulCat.
--   * Specification: https://docs.soliditylang.org/en/latest/yul.html#specification-of-yul-object
data YulObject = MkYulObject { yulObjectName :: String
                             , yulObjectCtor :: AnyYulCat  -- FIXME support constructor
                             , yulObjectSFns :: [ExportedFn] -- scoped functions
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
            -> [ExportedFn]
            -> YulObject
mkYulObject name ctor afns = MkYulObject { yulObjectName = name
                                         , yulObjectCtor = ctor
                                         , yulObjectSFns = afns
                                         , yulSubObjects = []
                                         }

emptyCtor :: AnyYulCat
emptyCtor = MkAnyYulCat (YulDis @Pure @())
