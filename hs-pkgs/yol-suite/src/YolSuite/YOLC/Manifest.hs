module YolSuite.YOLC.Manifest where

import           YulDSL.Core.YulObject (YulObject)

data DeploymentType = SingletonContract
                    | FactoryContract
                    | SharedLibrary
                    deriving Show

data Upgradability = NonUpgradable
                   | SingletonUpgradability
                   | BeaconUpgradability
                   deriving Show

data BuildUnit = MkBuildUnit { mainObject        :: YulObject
                             , deploymentType    :: DeploymentType
                             , upgradabilityMode :: Upgradability
                             } deriving Show

{- HLint ignore Manifest "Use newtype instead of data" -}
data Manifest = MkManifest { buildUnits      :: [BuildUnit]
                           -- , solidityVersion :: String
                           -- , evmVersion      :: String
                           } deriving Show
