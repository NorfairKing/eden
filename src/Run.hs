module Run where

import           Text.Printf           (printf)

import           System.Directory      (doesFileExist)
import           System.FilePath.Posix ((</>))

import           Constants
import           Eden
import           Paths
import           Solutions
import           Types
import           Utils

run :: EdenRun ()
run = do
    target <- askEden run_target
    runTarget target

runTarget :: RunTarget -> EdenRun ()
runTarget rt = do
    md <- solutionDir (run_target_problem rt) (run_target_language rt)
    let exec = md </> run_target_binary rt

    mInputPath <- case run_target_input rt of
                    Just rti -> return $ Just rti
                    Nothing  -> do
                        dif <- defaultInputFilePath $ run_target_problem rt
                        exists <- liftIO $ doesFileExist dif
                        if exists
                        then return $ Just dif
                        else return $ Nothing

    runSolution exec mInputPath



runSolution :: FilePath -- The absolute path to the executable to run
            -> Maybe FilePath -- The absolute path to the input file
            -> EdenRun ()
runSolution file minput = do
    let instr = case minput of
                    Nothing -> ""
                    Just i  -> unwords ["<", i]
    let cmd = unwords $
            [
                file
            ,   instr
            ]
    result <- runCommand cmd
    liftIO $ putStr result
