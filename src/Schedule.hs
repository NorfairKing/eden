module Schedule where

import           Types

schedule :: [Execution] -> Eden c ()
schedule = tell . inOrder

inOrder :: [Execution] -> ExecutionDependencyGraph
inOrder [] = []
inOrder [e] = [(Nothing, e)]
inOrder (e1:e2:es) = (Just e1, e2):inOrder (e2:es)
