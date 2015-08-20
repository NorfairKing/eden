module Run where

import           System.Directory      (doesFileExist)
import           System.FilePath.Posix ((</>))

import           Build
import           Eden
import           Schedule
import           Solutions
import           Types
import           Utils

run :: Target -> EdenRun ()
run TargetAll             = runAll
run TargetAllLibraries    = throwError "What did you think this would do? It doesn't make any sense."
run (TargetLibrary _)     = throwError "What did you think this would do? It doesn't make any sense."
run TargetAllProblems     = runAllProblems
run (TargetProblem p)     = runProblem p
run (TargetSolution p l)  = runSingleSolution p l

runAll :: EdenRun ()
runAll = runAllProblems

runAllProblems :: EdenRun ()
runAllProblems = do
    allProblems <- problems
    mapM_ runProblem allProblems

runProblem :: Problem -> EdenRun ()
runProblem p = do
    allSolutions <- solutions p
    mapM_ (runSingleSolution p) allSolutions

runSingleSolution :: Problem -> Language -> EdenRun ()
runSingleSolution p l = do
    bin <- askEden run_binary
    inp <- askEden run_input
    runSolution p l bin inp >>= schedule

runSolution :: Problem
              -> Language
              -> FilePath -- Binary
              -> Maybe FilePath -- Input
              -> Eden c [Execution]
runSolution p l bin inp = do
    bst <- buildSolution p l Nothing Nothing

    md <- solutionDir p l
    let cmd = md </> bin

    minput <- actualSolutionInput p inp
    let rst = RunExecution RunTarget {
              run_target_problem = p
            , run_target_language = l
            , run_target_bin = cmd
            , run_target_input = minput
            }
    return $ bst ++ [rst]
