module Execution where

import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           System.Directory         (doesDirectoryExist, doesFileExist)

import           Data.List                (delete, find)


import           Constants
import           Eden
import           Solutions
import           Types
import           Utils

executeForrest :: ExecutionForest -> EdenMake ()
executeForrest ef = do
    -- liftIO $ B.putStr $ encodePretty ef
    newTargets <- reduceForest $ execution_targets ef
    liftIO $ B.putStr $ encodePretty newTargets
    mapM_ executeExecutionTarget newTargets

--reduceForest' = return

executeExecutionTarget :: ExecutionTarget -> EdenMake ()
executeExecutionTarget et = do
    case execution et of
      MakeExecution mt    -> doMake mt
      RunExecution rt     -> doRunExecution rt >> return ()
      TestRunExecution rt -> doTestExecution rt
    mapM_ executeExecutionTarget $ execution_dependants et

reduceForest :: [ExecutionTarget] -> EdenMake [ExecutionTarget]
reduceForest ts = do
      rd <- doReduction ts
      if ts == rd
      then return ts
      else reduceForest rd
  where
    doReduction :: [ExecutionTarget] -> EdenMake [ExecutionTarget]
    doReduction [] = return []
    doReduction ets = do
        -- ets <- mapM easyReduction etss
        case find sameExecution (ets `x` ets) of
              Just (t1, t2) -> do -- combine bottom level targets
                  let sameTarget = ExecutionTarget {
                        execution = execution t1
                      , execution_dependants = execution_dependants t1 ++ execution_dependants t2
                    }
                  let newTargets = (++ [sameTarget]) $ delete t1 $ delete t2 ets
                  return newTargets
              Nothing -> do -- go one level deper
                  (flip mapM) ets $ \et -> do
                      reduced <- doReduction $ execution_dependants et
                      return $ ExecutionTarget {
                          execution = execution et
                        , execution_dependants = reduced
                        }
    sameExecution :: (ExecutionTarget, ExecutionTarget) -> Bool
    sameExecution (et1, et2) = execution et1 == execution et2
    easyReduction :: ExecutionTarget -> EdenMake ExecutionTarget
    easyReduction et = do
          newDependants <- (flip mapM) deps $ \eto -> do
                            etor <- easyReduction eto
                            if sameExecution (et, etor)
                            then return $ execution_dependants etor --skip etor
                            else return $ [etor]
          return $ ExecutionTarget {
                    execution = eet
                  , execution_dependants = concat newDependants
                }
      where
        eet = execution et
        deps = execution_dependants et



x :: Eq a => [a] -> [a] -> [(a,a)]
x as bs = [(a,b) | a <- as, b <- bs, a /= b]

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

