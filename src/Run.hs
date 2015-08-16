module Run where

import           System.Directory      (doesFileExist)
import           System.FilePath.Posix ((</>))

import           Eden
import           Solutions
import           Types
import           Utils

run :: Target -> EdenRun ()
run TargetAll             = runAll
run TargetAllLibraries    = throwError "What did you think this would do? It doesn't make any sense."
run TargetAllProblems     = runAllProblems
run (TargetProblem p)     = runProblem p
run (TargetSolution p l)  = runSolution p l

runAll :: EdenRun ()
runAll = runAllProblems

runAllProblems :: EdenRun ()
runAllProblems = do
    allProblems <- problems
    mapM_ runProblem allProblems

runProblem :: Problem -> EdenRun ()
runProblem p = do
    allSolutions <- solutions p
    mapM_ (runSolution p) allSolutions

runSolution :: Problem -> Language -> EdenRun ()
runSolution p l = do
    md <- solutionDir p l
    bin <- askEden run_binary
    inp <- askEden run_input
    let cmd = md </> bin

    minput <- case inp of
                    Just rti -> return $ Just rti
                    Nothing  -> do
                        dif <- defaultInputFilePath p
                        exists <- liftIO $ doesFileExist dif
                        if exists
                        then return $ Just dif
                        else return $ Nothing

    printIf (askGlobal opt_commands) cmd
    result <- case minput of
        Nothing  -> runCommand cmd
        Just inf -> runCommandWithInput cmd inf
    liftIO $ putStr $ unwords [problemDirName p, padNWith 8 ' ' l ++ ":", result]
