module YulDSL.Core.YulObject where

import           Data.List          (intercalate)
import           YulDSL.Core.YulCat (AnyFn, YulCat (..), YulCode (..))

-- | A Yul Object per spec.
--
-- Note:
--   * Do not confuse this with YulObj which is an "object" in the category of YulCat.
--   * Specification: https://docs.soliditylang.org/en/latest/yul.html#specification-of-yul-object
data YulObject = MkYulObject { yulObjectName :: String
                             , yulObjectCode :: YulCode
                             , yulSubObjects :: [YulObject]
                             -- , TODO support object data
                             }

instance Show YulObject where
  show o = "-- Functions:\n\n"
           <> intercalate "\n\n" (fmap show (yulFunctions.yulObjectCode $ o))
           <> "\n\n-- Init code:\n\n"
           <> (show.yulInitCode.yulObjectCode) o

exportFunctions :: String -> [AnyFn] -> YulObject
exportFunctions name fns = MkYulObject name (MkYulCode fns create_dispatcher) []
  where create_dispatcher = YulId -- TODO
  -- TODO dependency analysis of fns
