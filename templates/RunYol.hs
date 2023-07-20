{-# LANGUAGE NoImplicitPrelude #-}

import           BasePrelude
import qualified Data.Text                as T

import qualified YulDSL.CodeGen.PlantUML
import YulDSL.Core (Fn (..))

import __YOL_MOD_NAME__

type Compiler = forall a b.  Fn a b -> String

default (String)

compilers :: [Compiler]
compilers = [ \(Defun name cat) -> "# __YOL_MOD_NAME__." <> name <> "\n\n" ++
                           show cat <> "\n\n" ++
                           "# " <> replicate 98 '-' <> "\n"
            , \(Defun name cat) -> T.unpack (YulDSL.CodeGen.PlantUML.compile name cat) ++
                           "' " <> replicate 98 '-' <> "\n"
            ]

main :: IO ()
main = do
  __COMPILE_SYMBOLS__
  return ()
