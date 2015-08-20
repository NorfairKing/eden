module Schedule where

import           Types

inOrder :: [ExecutionTarget] -> ExecutionTarget
inOrder [] = error "no targets in order?"
inOrder [et] = et
inOrder (e:es) = e `before` inOrder es

before :: ExecutionTarget -> ExecutionTarget -> ExecutionTarget
before et1 et2 = ExecutionTarget {
        execution = execution et1
      , execution_dependants = execution_dependants et1 ++ [et2]
    }

schedule :: ExecutionTarget -> Eden c ()
schedule et = tell $ ExecutionForest { execution_targets = [et] }

scheduleSeparately :: [ExecutionTarget] -> Eden c ()
scheduleSeparately ets = tell $ ExecutionForest { execution_targets = ets }
