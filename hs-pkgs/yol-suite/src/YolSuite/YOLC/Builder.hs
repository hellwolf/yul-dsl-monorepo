{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-|

Copyright   : (c) 2023-2024 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : hellwolf@yolc.dev
Stability   : experimental

= Description

Manifest builder.

-}
module YolSuite.YOLC.Builder
  ( Manifest (..)
  , buildManifest
  ) where

-- base
import           Data.Either             (partitionEithers)
import           Data.Functor            ((<&>))
import           Data.Maybe              (fromJust, fromMaybe, mapMaybe)
import           Data.String             (fromString)
-- text
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
-- lens
import           Control.Lens            ((^..), (^?))
-- aseson
import           Data.Aeson              (KeyValue ((.=)))
import qualified Data.Aeson              as Aeson
import qualified Data.Aeson.Types        as AesonTypes
-- aeson-lens
import           Data.Aeson.Lens         (key, values)
-- system-process
import           System.IO               (hClose, hGetContents', hPutStr)
import           System.Process          (CreateProcess (..), StdStream (CreatePipe), createProcess, proc)
--
import           YulDSL.CodeGens.YulGen
import           YulDSL.Core
--
import           YolSuite.TH
import           YolSuite.YOLC.Manifest


type BuildResult = Either T.Text T.Text

-- | Compile a YulObject to bytecode.
compile_main_object :: YulObject -> IO BuildResult
compile_main_object mo = do
  let oname = yulObjectName mo
  solc <- get_solc
  (Just hin, Just hout, _, _) <- createProcess (proc solc ["--standard-json"])
                                 { std_in = CreatePipe, std_out = CreatePipe }
  hPutStr hin (T.unpack . decodeUtf8 . Aeson.encode $ Aeson.object
               [ "language" .= ("Yul" :: String)
               , "sources"  .= Aeson.object [
                   "main.yul" .= Aeson.object [ "content" .= compileYulObject mo ]
                   ]
               , "settings" .= Aeson.object [
                   "evmVersion" .= ("paris" :: String),
                   "outputSelection" .= Aeson.object [
                       "*" .= Aeson.object [ "*" .= (["evm.bytecode.object"] :: [String])]
                       ]
                   ]
                 -- "optimizer": { "enabled": true, "details": { "yul": true } }
               ])
  hClose hin
  maybeOut :: Maybe Aeson.Value <- Aeson.decode . encodeUtf8 . T.pack <$> hGetContents' hout
  return $ run_maybe "Failed to decode json output from solc." maybeOut
    $ \out -> run_maybe "Missing 'errors' field." (out ^? key "errors" >>= Just . (^.. values))
              $ \errors ->
                  if null errors
                  then run_maybe
                       "Missing 'contracts...evm.bytecode.object' field."
                       (out
                         ^? key "contracts"
                         >>= (^? key "main.yul")
                         >>= (^? key (fromString oname))
                         >>= (^? key "evm")
                         >>= (^? key "bytecode")
                         >>= (^? key "object"))
                       stringify
                  else Left $
                       foldr
                       (((<>)
                          . fromJust . AesonTypes.parseMaybe AesonTypes.parseJSON
                          . fromMaybe (fromString "(empty error message)")
                        )
                        . (^? key "formattedMessage")
                       )
                       (fromString "")
                       errors

ifunc_name :: ScopedFn -> Maybe T.Text
ifunc_name (ExternalFn perm (SELECTOR (_, Just(FuncSig (fname, _)))) (_ :: FnCat eff a b))
  = Just
    $ "  function " <> T.pack fname
    <> "(" <> T.intercalate ", " argTypes <>") external" <> effect perm
    <> (if null retTypes then ";" else " returns (" <> T.intercalate ", " retTypes <> ");")
    where argTypes = map (T.pack . abiCoreTypeCanonName) (abiTypeInfo @a)
          retTypes = map (T.pack . abiCoreTypeCanonName) (abiTypeInfo @b)
          effect NoStorageAccess         = " pure"
          effect ReadOnlyExternalStorage = " view"
          effect _                       = ""
ifunc_name _ = Nothing

-- Compile interface of the build unit.
build_interface :: BuildUnit -> IO BuildResult
build_interface (MkBuildUnit { mainObject = MkYulObject
                               { yulObjectName = oname
                               , yulObjectSFns = sfns
                               } }) =
  let iname = "I" ++ oname ++ "Program"
  in pure $ Right
     (T.unlines $
      [ "interface " <> T.pack iname <> " {"
      ] ++
      mapMaybe ifunc_name sfns ++
      [
      "}"
      ])

-- Compile program of the build unit.
build_program :: BuildUnit -> IO BuildResult
build_program (MkBuildUnit { mainObject = mo }) = do
  result <- compile_main_object mo
  pure $ result <&> \bytecode ->
    let oname = yulObjectName mo
        iname = "I" ++ oname ++ "Program"
        pname = oname ++ "Program"
        bytecode' = foldr ((<>) . ("\\x" <>)) "" (T.chunksOf 2 bytecode)
    in singleton_contract_template (pname, iname, bytecode')

-- Build manifest in the single-file output mode.
buildManifest :: Manifest -> IO BuildResult
buildManifest (MkManifest { buildUnits }) = do
  sections <- sequence (concatMap (\bu -> [build_interface bu, build_program bu]) buildUnits)
  let (errors, codes) = partitionEithers (Right project_preamble_template : sections)
  pure $ if null errors then Right (T.intercalate "\n" codes)
         else Left (T.intercalate "\n" errors)

--
-- Internal founctions
--

get_solc :: IO String
get_solc = return "solc" -- FIXME read SOLC environment variable

run_maybe :: T.Text -> Maybe a -> (a -> BuildResult) -> BuildResult
run_maybe err  Nothing _ = Left err
run_maybe _ (Just val) f = f val

stringify :: Aeson.Value -> BuildResult
stringify val = case Aeson.fromJSON val of
  Aeson.Success str -> Right str
  Aeson.Error   err -> Left (T.pack err)

project_preamble_template :: T.Text
project_preamble_template = [fmt'|
#include "../../../templates/Preamble.sol"
|]

singleton_contract_template :: (String, String, T.Text) -> T.Text
singleton_contract_template (pname, iname, bytecode) = [fmt'|
#include "../../../templates/SingletonContract.sol"
|]
