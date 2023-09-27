{-# LANGUAGE TemplateHaskell #-}

module YolSuite.TH (fmt') where

import           Language.Haskell.TH.Quote (QuasiQuoter)
import           PyF                       (addFormatting, mkFormatter, strConfig)
import           PyF.Internal.QQ           (Config (..))


-- | CPP outputs lines starting with '#', removing them from QQ.
remove_cpp :: String -> String
remove_cpp = unlines . filter (\l -> case l of '#':_ -> False; _ -> True;) . lines

add_remove_cpp :: Config -> Config
add_remove_cpp config = config { postProcess = \q -> postProcess config [| remove_cpp $(q) |] }

fmt' :: QuasiQuoter
fmt' = mkFormatter "fmt'" ( add_remove_cpp . addFormatting ('%', '%') $ strConfig )
