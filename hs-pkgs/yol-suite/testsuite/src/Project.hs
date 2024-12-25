module Project where

import qualified Basic_Tests
import qualified Num_Tests
import           YolSuite.YOLC.Manifest

manifest :: Manifest
manifest = MkManifest
  { buildUnits = [ MkBuildUnit { mainObject = Basic_Tests.object
                               , deploymentType = SingletonContract
                               , upgradabilityMode = NonUpgradable
                               }
                 , MkBuildUnit { mainObject = Num_Tests.object
                               , deploymentType = SingletonContract
                               , upgradabilityMode = NonUpgradable
                               }
                 ]
  }
