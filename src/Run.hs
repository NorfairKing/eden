module Run where

import           System.Directory      (doesFileExist)
import           System.FilePath.Posix ((</>))

import           Eden
import           Solutions
import           Types
import           Utils

run :: Target -> EdenRun ()
run TargetAll             = notImplementedYet
run TargetAllLibraries    = notImplementedYet
run TargetAllProblems     = notImplementedYet
run (TargetProblem _)     = notImplementedYet
run (TargetSolution p l)  = do
    md <- solutionDir p l
    bin <- askEden run_binary
    inp <- askEden run_input
    let exec = md </> bin

    mInputPath <- case inp of
                    Just rti -> return $ Just rti
                    Nothing  -> do
                        dif <- defaultInputFilePath p
                        exists <- liftIO $ doesFileExist dif
                        if exists
                        then return $ Just dif
                        else return $ Nothing

    runSolution exec mInputPath



runSolution :: FilePath -- The absolute path to the executable to run
            -> Maybe FilePath -- The absolute path to the input file
            -> EdenRun ()
runSolution file minput = do
    let cmd = file
    printIf (askGlobal opt_commands) cmd
    result <- case minput of
        Nothing  -> runCommand cmd
        Just inf -> runCommandWithInput cmd inf
    liftIO $ putStr result
