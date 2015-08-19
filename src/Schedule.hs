module Schedule where

import           Types

inOrder :: [ExecutionTarget] -> ExecutionTarget
inOrder [] = error "no targets in order?"
inOrder [et] = et
inOrder (e:es) = inOrder es `after` e

after :: ExecutionTarget -> ExecutionTarget -> ExecutionTarget
after et1 et2 = et3
  where et3 = ExecutionTarget {
        execution = execution et2
      , execution_dependants = ExecutionTarget {
              execution = execution et1
            , execution_dependants = execution_dependants et1
          }:execution_dependants et2
    }

schedule :: ExecutionTarget -> Eden c ()
schedule et = tell $ ExecutionForest { execution_targets = [et] }

scheduleSeparately :: [ExecutionTarget] -> Eden c ()
scheduleSeparately ets = tell $ ExecutionForest { execution_targets = ets }
