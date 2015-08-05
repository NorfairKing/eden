module TH where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote


litFile :: QuasiQuoter
litFile = quoteFile lit

lit :: QuasiQuoter
lit = QuasiQuoter { quoteExp = literally }

literally :: String -> Q Exp
literally = return . LitE . StringL

