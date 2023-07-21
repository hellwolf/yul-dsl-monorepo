module YulDSL.Core.YulObject where

import           YulDSL.Core.YulCat (YulCode)

-- | A Yul Object per spec.
--
-- Note:
--   * Do not confuse this with YulObj which is an "object" in the category of YulCat.
--   * Specification: https://docs.soliditylang.org/en/latest/yul.html#specification-of-yul-object
data YulObject = MkYulObject { yulObjectName :: String
                             , yulObjectCode :: YulCode
                             , yulSubObjects :: [YulObject]
                             -- , TODO support object data
                             } deriving Show
