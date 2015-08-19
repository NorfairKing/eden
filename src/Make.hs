module Make where

import           Types


make :: FilePath     -- Make directory
     -> FilePath     -- Make file
     -> Maybe String -- Make rule
     -> ExecutionTarget
make dir mf mr = makeExecutionTarget mt
  where mt = MakeTarget {
            make_dir  = dir
          , make_file = mf
          , make_rule = mr
        }


makeExecutionTarget :: MakeTarget -> ExecutionTarget
makeExecutionTarget mt = ExecutionTarget {
        execution = MakeExecution mt
      , execution_dependants = []
    }
