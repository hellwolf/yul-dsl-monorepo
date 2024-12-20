module Project where

import qualified Num_Tests
import           YolSuite.YOLC.Manifest

manifest :: Manifest
manifest = MkManifest
  { buildUnits = [ MkBuildUnit { mainObject = Num_Tests.object
                               , deploymentType = SingletonContract
                               , upgradabilityMode = NonUpgradable
                               }
                 ]
  }
