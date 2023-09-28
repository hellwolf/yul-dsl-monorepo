{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module YolSuite.YOLC.Builder
  ( Manifest (..)
  , buildManifest
  ) where

import           Control.Lens            ((^..), (^?))
import           Control.Monad           (foldM)
import           Data.Aeson
    ( KeyValue ((.=))
    , Result (Error, Success)
    , Value
    , decode
    , encode
    , fromJSON
    , object
    )
import           Data.Aeson.Lens         (key, values)
import           Data.String             (fromString)
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           System.IO               (hClose, hGetContents', hPutStr)
import           System.Process          (CreateProcess (..), StdStream (CreatePipe), createProcess, proc)
--
import qualified YulDSL.CodeGen.Yul      as YulCodeGen
import qualified YulDSL.Core             as YulDSLCore
--
import           YolSuite.TH
import           YolSuite.YOLC.Manifest

get_solc :: IO String
get_solc = return "solc" -- FIXME read SOLC environment variable

type BuildResult = Either T.Text T.Text

run_maybe :: Maybe a -> T.Text -> (a -> BuildResult) -> BuildResult
run_maybe Nothing err _  = Left err
run_maybe (Just val) _ f = f val

stringify :: Value -> BuildResult
stringify val = case fromJSON val of
    Success str -> Right str
    Error   err -> Left (T.pack err)

-- | Compile a YulObject to bytecode.
compile_mo :: YulDSLCore.YulObject -> IO BuildResult
compile_mo mo = do
  let oname = YulDSLCore.yulObjectName mo
  solc <- get_solc
  (Just hin, Just hout, _, _) <- createProcess (proc solc ["--standard-json"])
                                 { std_in = CreatePipe, std_out = CreatePipe }
  hPutStr hin (T.unpack . decodeUtf8 . encode $ object
               [ "language" .= ("Yul" :: String)
               , "sources"  .= object [
                   "main.yul" .= object [ "content" .= YulCodeGen.compileObject mo ]
                   ]
               , "settings" .= object [
                   "evmVersion" .= ("paris" :: String),
                   "outputSelection" .= object [
                       "*" .= object [ "*" .= (["evm.bytecode.object"] :: [String])]
                       ]
                   ]
                 -- "optimizer": { "enabled": true, "details": { "yul": true } }
               ])
  hClose hin
  decode <$> encodeUtf8 <$> T.pack <$> hGetContents' hout >>= \(maybeOut :: Maybe Value) -> return $
    run_maybe maybeOut "Failed to decode json output from solc." (
    \out -> run_maybe (out ^? key "errors" >>= Just . (^.. values)) "Missing 'errors' field." (
      \errors -> if length errors == 0
                 then run_maybe (out ^? key "contracts"
                                       >>= (^? key "main.yul")
                                       >>= (^? key (fromString oname))
                                       >>= (^? key "evm")
                                       >>= (^? key "bytecode")
                                       >>= (^? key "object"))
                      "Missing 'contracts...evm.bytecode.object' field."
                      stringify
                 else Left (decodeUtf8 . encode $ errors)
      )
    )

-- | Compile one build unit.
build_bu :: BuildUnit -> IO BuildResult
build_bu (MkBuildUnit { mainObject = mo }) = do
  let oname = YulDSLCore.yulObjectName mo
      pname = oname ++ "Program"
  result <- compile_mo mo
  case result of
    Left err -> return (Left err)
    Right bytecode' -> let bytecode = foldr ((<>) . ("\\x" <>)) "" (T.chunksOf 2 bytecode')
                       in return (Right [fmt'|
#include "../../../templates/SingletonContract.sol"
|])

-- | Build manifest in the single-file output mode.
buildManifest :: Manifest -> IO BuildResult
buildManifest (MkManifest as) = foldM
  (\b a -> case b of
      Left b'  -> return (Left b')
      Right b' -> build_bu a >>= return . fmap (\a' -> b' <> a' <> "\n"))
  (Right [fmt'|// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.20;
|])
  as
