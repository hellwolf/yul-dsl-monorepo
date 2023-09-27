module YolSuite.YOLC.Manifest where

import           YulDSL.Core.YulObject (YulObject)

data DeploymentType = SingletonContract
                    | FactoryContract
                    | SharedLibrary
                    deriving Show

data Upgradability = NonUpgradable
                   | GrandfatherlyUpgradable
                   | FullUpgradable
                   deriving Show

data BuildUnit = MkBuildUnit { mainObject        :: YulObject
                             , deploymentType    :: DeploymentType
                             , upgradabilityMode :: Upgradability
                             } deriving Show

data Manifest = MkManifest { buildUnits :: [BuildUnit]
                           } deriving Show
