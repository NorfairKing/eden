module Execution where

import           System.Directory (doesDirectoryExist, doesFileExist)

import           Constants
import           Eden
import           Solutions
import           Types
import           Utils

executeForrest :: ExecutionForest -> EdenMake ()
executeForrest ef = do
    rf <- reduceForrest ef
    mapM_ executeExecutionTarget $ execution_targets ef

executeExecutionTarget :: ExecutionTarget -> EdenMake ()
executeExecutionTarget et = do
    case execution et of
      MakeExecution mt    -> doMake mt
      RunExecution rt     -> doRunExecution rt >> return ()
      TestRunExecution rt -> doTestExecution rt
    mapM_ executeExecutionTarget $ execution_dependants et

reduceForrest :: ExecutionForest -> EdenMake ExecutionForest
reduceForrest = return

doMake :: MakeTarget -> EdenMake ()
doMake mt = do
    let dir = make_dir mt
        makefile = make_file mt
        mrule = make_rule mt
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

doRunExecution :: RunTarget -> EdenMake Int
doRunExecution rt = do
    let cmd = run_target_bin rt
    let p = run_target_problem rt
    let l = run_target_language rt
    printIf (askGlobal opt_commands) cmd
    result <- case run_target_input rt of
        Nothing  -> runCommand cmd
        Just inf -> runCommandWithInput cmd inf
    liftIO $ putStr $ unwords ["Run: ", problemDirName p, padNWith 8 ' ' l ++ ":", result]
    return $ read result

doTestExecution :: TestTarget -> EdenMake ()
doTestExecution tt = do
    let p = test_target_problem tt
    let l = test_target_language tt
    actual <- doRunExecution RunTarget {
            run_target_problem = p
          , run_target_language = l
          , run_target_bin = test_target_bin tt
          , run_target_input = test_target_input tt
        }
    dof <- defaultOutputFilePath p
    expected <- readFromFile dof
    let same = ["Test:", problemDirName p, padNWith 8 ' ' l ++ ":"]
    if actual /= expected
    then          throwError $ unwords $ same ++ ["Fail,", "Expected:", show expected, "Actual:", show actual]
    else liftIO $ putStrLn   $ unwords $ same ++ ["Success."]

