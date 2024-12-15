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
import           Control.Monad           (foldM)
import           Data.Functor            ((<&>))
import           Data.Maybe              (fromMaybe)
import           Data.String             (fromString)
-- text
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
-- lens
import           Control.Lens            ((^..), (^?))
-- aseson
import           Data.Aeson              (KeyValue ((.=)))
import qualified Data.Aeson              as Aeson
-- aeson-lens
import           Data.Aeson.Lens         (key, values)
-- system-process
import           System.IO               (hClose, hGetContents', hPutStr)
import           System.Process          (CreateProcess (..), StdStream (CreatePipe), createProcess, proc)
--
import qualified YulDSL.CodeGens.YulGen  as YulCodeGen
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

stringify :: Aeson.Value -> BuildResult
stringify val = case Aeson.fromJSON val of
  Aeson.Success str -> Right str
  Aeson.Error   err -> Left (T.pack err)

-- | Compile a YulObject to bytecode.
compile_mo :: YulDSLCore.YulObject -> IO BuildResult
compile_mo mo = do
  let oname = YulDSLCore.yulObjectName mo
  solc <- get_solc
  (Just hin, Just hout, _, _) <- createProcess (proc solc ["--standard-json"])
                                 { std_in = CreatePipe, std_out = CreatePipe }
  hPutStr hin (T.unpack . decodeUtf8 . Aeson.encode $ Aeson.object
               [ "language" .= ("Yul" :: String)
               , "sources"  .= Aeson.object [
                   "main.yul" .= Aeson.object [ "content" .= YulCodeGen.compileObject mo ]
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
  return $ run_maybe maybeOut "Failed to decode json output from solc."
    $ \out -> run_maybe (out ^? key "errors" >>= Just . (^.. values)) "Missing 'errors' field."
              $ \errors ->
                  if null errors
                  then run_maybe (out ^? key "contracts"
                                   >>= (^? key "main.yul")
                                   >>= (^? key (fromString oname))
                                   >>= (^? key "evm")
                                   >>= (^? key "bytecode")
                                   >>= (^? key "object"))
                       "Missing 'contracts...evm.bytecode.object' field."
                       stringify
                  else Left $
                       foldr
                       ((<>) . fromMaybe (fromString "(empty error message)") . Aeson.decode . Aeson.encode)
                       (fromString "")
                       (map (^? key "formattedMessage") errors)

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
buildManifest (MkManifest { buildUnits = bus }) = foldM
  (\b a -> case b of
      Left b'  -> return (Left b')
      Right b' -> build_bu a <&> fmap (\a' -> b' <> a' <> "\n"))
  (Right [fmt'|
#include "../../../templates/Preamble.sol"
|])
  bus
