module Utils where

import           System.Directory (doesDirectoryExist, doesFileExist)
import           System.Exit      (ExitCode (..))
import           System.IO        (hGetContents)
import           System.Process   (createProcess, readProcess,
                                   runInteractiveCommand, shell, system,
                                   waitForProcess)
import           System.TimeIt    (timeItT)

import           Eden
import           Types

runCommandWithInput :: String -> FilePath -> Eden c String
runCommandWithInput str inf = do
    instr <- liftIO $ readFile inf
    liftIO $ readProcess bin args instr
  where (bin:args) = words str


runCommand :: String -> Eden c String
runCommand str = liftIO $ readProcess bin args ""
  where (bin:args) = words str

runRaw :: String -> Eden c ()
runRaw cmd = do
    printIf (askGlobal opt_commands) cmd
    (inh, outh, errh, ph) <- liftIO $ runInteractiveCommand cmd
    ec <- liftIO $ waitForProcess ph
    case ec of
        ExitSuccess -> return ()
        ExitFailure code -> do
            out   <- liftIO $ hGetContents outh
            err   <- liftIO $ hGetContents errh
            throwError $ unlines $
                [
                    unwords ["Command", show cmd, "exited with code", show code]
                ,   out
                ,   err
                ]

printIf :: Eden c Bool -> String -> Eden c ()
printIf bool str = do
    b <- bool
    if b
    then liftIO $ putStrLn str
    else return ()


make :: FilePath     -- Make directory
     -> FilePath     -- Make file
     -> Maybe String -- Make rule
     -> Eden c ()
make dir makefile mrule = do

    dirExists   <- liftIO $ doesDirectoryExist dir
    fileExists <- liftIO $ doesFileExist makefile

    if not dirExists
    then throwError $ unwords ["Directory", dir, "does not exist."]
    else do
        if not fileExists
        then throwError $ unwords ["Makefile", makefile, "does not exist."]
        else do
            let rulestr = case mrule of
                        Nothing -> ""
                        Just rule -> rule
            let cmd = unwords $
                    [
                        "make"
                    ,   "--directory"   , dir
                    ,   "--file"        , makefile
                    ,   "--jobs"
                    ,   rulestr
                    ]
            runRaw cmd

notImplementedYet :: Eden c ()
notImplementedYet = throwError "This feature is not implemented yet."
