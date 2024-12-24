module YulDSL.CodeGens.Yul.Internal.BuiltIns where
import           YulDSL.CodeGens.Yul.Internal.BuiltInRegistra     (BuiltInEntry)
--
import qualified YulDSL.CodeGens.Yul.Internal.BuiltIns.ABICodec
import qualified YulDSL.CodeGens.Yul.Internal.BuiltIns.Arithmetic
import qualified YulDSL.CodeGens.Yul.Internal.BuiltIns.Runtime
import qualified YulDSL.CodeGens.Yul.Internal.BuiltIns.ValueType

default_builtins :: [BuiltInEntry]
default_builtins =
  YulDSL.CodeGens.Yul.Internal.BuiltIns.Runtime.exports ++
  YulDSL.CodeGens.Yul.Internal.BuiltIns.ValueType.exports ++
  YulDSL.CodeGens.Yul.Internal.BuiltIns.Arithmetic.exports ++
  YulDSL.CodeGens.Yul.Internal.BuiltIns.ABICodec.exports
