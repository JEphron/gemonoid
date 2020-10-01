module Raw where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

rw :: QuasiQuoter
rw = QuasiQuoter {
    quoteExp  = return . LitE . StringL
}
