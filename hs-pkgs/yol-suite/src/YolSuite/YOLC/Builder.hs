{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module YolSuite.YOLC.Builder
  ( Manifest (..)
  , buildManifest
  ) where

import           Control.Lens.Fold       ((^?))
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Maybe              (fromJust)
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

stringify :: Value -> Maybe T.Text
stringify val = case fromJSON val of
    Success str -> Just str
    Error _     -> Nothing

-- | Compile a YulObject to bytecode.
compile_mo :: YulDSLCore.YulObject -> IO T.Text
compile_mo mo = do
  let oname = YulDSLCore.yulObjectName mo
  solc <- get_solc
  (Just hin, Just hout, _, _) <- createProcess (proc solc ["--standard-json"])
                                 { std_in = CreatePipe, std_out = CreatePipe }
  hPutStr hin (T.unpack $ decodeUtf8 $ encode $ object
               [ "language" .= ("Yul" :: String)
               , "sources"  .= object [
                   "main.yul" .= object [ "content" .= YulCodeGen.compileObject mo ]
                   ]
               , "settings" .= object [
                   "outputSelection" .= object [
                       "*" .= object [ "*" .= (["evm.bytecode.object"] :: [String])]
                       ]
                   ]
               ])
  hClose hin
  out :: Maybe Value <- decode <$> encodeUtf8 <$> T.pack <$> hGetContents' hout
  print out
  -- TODO handle errors
  return $ fromJust $ out
    >>= (^? key "contracts")
    >>= (^? key "main.yul")
    >>= (^? key (fromString oname))
    >>= (^? key "evm")
    >>= (^? key "bytecode")
    >>= (^? key "object")
    >>= stringify

-- | Compile one build unit.
build_bu :: BuildUnit -> IO T.Text
build_bu (MkBuildUnit { mainObject = mo }) = do
  let oname = YulDSLCore.yulObjectName mo
      pname = oname ++ "Program"
  bytecode <- compile_mo mo
  return $ [fmt'|
#include "../../../templates/SingletonContract.sol"
|]

-- | Build manifest in the single-file output mode.
buildManifest :: Manifest -> IO T.Text
buildManifest (MkManifest as) = mapM build_bu as >>= return . T.intercalate "\n"
