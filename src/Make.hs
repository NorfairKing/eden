module Make where

import           Types


make :: FilePath     -- Make directory
     -> FilePath     -- Make file
     -> Maybe String -- Make rule
     -> Execution
make dir mf mr = MakeExecution $ MakeTarget {
            make_dir  = dir
          , make_file = mf
          , make_rule = mr
        }
