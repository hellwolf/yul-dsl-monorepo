{-# LANGUAGE TemplateHaskell #-}

module YOLC.TH (fmt') where

import           Language.Haskell.TH.Quote (QuasiQuoter)
import           PyF
import           PyF.Internal.QQ           (Config (..))

remove_cpp :: String -> String
remove_cpp = unlines . filter (\l -> case l of '#':_ -> False; _ -> True;) . lines

add_remove_cpp :: Config -> Config
add_remove_cpp config = config { postProcess = \q -> postProcess config [| remove_cpp $(q) |] }

fmt' :: QuasiQuoter
fmt' = mkFormatter "fmt'" ( add_remove_cpp . addFormatting ('%', '%') $ strConfig)
