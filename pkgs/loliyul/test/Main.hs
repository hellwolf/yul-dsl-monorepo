{-# OPTIONS_GHC -Wno-missing-signatures #-}

import qualified Data.Text                as T

import qualified LoliYul.CodeGen.PlantUML
import           LoliYul.Examples.Basic


--------------------------------------------------------------------------------
-- Test Run Interface
--------------------------------------------------------------------------------

compilers = [ \name cat -> "# " <> name <> ":\n\n" ++
                           show cat <> "\n" ++
                           replicate 80 '-' <> "\n"
            , \name cat -> T.unpack (LoliYul.CodeGen.PlantUML.compile name cat) ++
                           replicate 80 '-' <> "\n"
            ]

main = do
  let n = 0
      c = compilers !! n
  putStr $ c "example_id" simple_id
  putStr $ c "const42" const_42
  putStr $ c "foo" foo
  putStr $ c "foo2" foo2
  putStr $ c "ERC20.transfer" erc20_transfer
