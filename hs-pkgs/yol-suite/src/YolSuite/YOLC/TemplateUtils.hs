module YolSuite.YOLC.TemplateUtils where
import           Language.Haskell.TH.Quote (QuasiQuoter)
import           PyF                       (addFormatting, mkFormatter, strConfig)

fmt :: QuasiQuoter
fmt = mkFormatter "fmt" ( addFormatting ('%', '%') $ strConfig )
