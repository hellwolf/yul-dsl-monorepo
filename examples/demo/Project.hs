module Project where

import qualified Basic
import qualified ERC20
import           YolSuite.YOLC.Manifest

manifest :: Manifest
manifest = MkManifest
  { buildUnits = [ MkBuildUnit { mainObject = ERC20.object
                               , deploymentType = SingletonContract
                               , upgradabilityMode = NonUpgradable
                               }
                 , MkBuildUnit { mainObject = Basic.object
                               , deploymentType = SingletonContract
                               , upgradabilityMode = NonUpgradable
                               }
                 ]
  }
