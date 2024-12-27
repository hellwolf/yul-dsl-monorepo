module YulDSL.CodeGens.Yul.Internal.BuiltIns where
import YulDSL.CodeGens.Yul.Internal.BuiltInRegistra     (BuiltInEntry)
--
import YulDSL.CodeGens.Yul.Internal.BuiltIns.ABICodec qualified
import YulDSL.CodeGens.Yul.Internal.BuiltIns.Arithmetic qualified
import YulDSL.CodeGens.Yul.Internal.BuiltIns.Runtime qualified
import YulDSL.CodeGens.Yul.Internal.BuiltIns.ValueType qualified

default_builtins :: [BuiltInEntry]
default_builtins =
  YulDSL.CodeGens.Yul.Internal.BuiltIns.Runtime.exports ++
  YulDSL.CodeGens.Yul.Internal.BuiltIns.ValueType.exports ++
  YulDSL.CodeGens.Yul.Internal.BuiltIns.Arithmetic.exports ++
  YulDSL.CodeGens.Yul.Internal.BuiltIns.ABICodec.exports
