module Execution where

import           System.Directory (doesDirectoryExist, doesFileExist)

import           Data.List        (nub, sort)
import           Data.Map         (Map)
import qualified Data.Map         as M
import           Data.Maybe       (fromJust)
import           Data.Tree        (Forest, Tree (..), levels)

import           Eden
import           Solutions
import           Types
import           Utils

executeGraph :: ExecutionDependencies -> EdenMake ()
executeGraph ef = executeForest . graphToForest $ toGraph ef

executeForest :: ExecutionForest -> EdenMake ()
executeForest = mapM_ (mapM_ execute . sort) . aggregate . map levels

aggregate :: [[[a]]] -> [[a]]
aggregate [] = []
aggregate [t] = t
aggregate (t1:t2:ts) = aggregate $ (aggregateTrees t1 t2):ts

aggregateTrees :: [[a]] -> [[a]] -> [[a]]
aggregateTrees [] [] = []
aggregateTrees x  [] = x
aggregateTrees [] y  = y
aggregateTrees (t1:ts1) (t2:ts2) = (t1 ++ t2):(aggregateTrees ts1 ts2)

execute :: Execution -> EdenMake ()
execute (MakeExecution mt)    = doMake mt
execute (RunExecution rt)     = doRunExecution rt >> return ()
execute (TestRunExecution rt) = doTestExecution rt

toGraph :: ExecutionDependencies -> ExecutionDependencyGraph
toGraph edg = addAll edg startingMap
  where
    startingMap :: Map Execution [Execution]
    startingMap = M.fromList $ zip (allExecutions edg) $ repeat []

    -- Map to dependencies: (a -> b) means b has to happen before a
    addAll :: ExecutionDependencies -> ExecutionDependencyGraph -> ExecutionDependencyGraph
    addAll [] accMap                  = accMap
    addAll ((Nothing, _): es) accMap  = addAll es accMap
    addAll ((Just bf, af):es) accMap  = addAll es $ M.update fn af accMap
      where
        fn :: [Execution] -> Maybe [Execution]
        fn es = Just (bf:es)

allExecutions :: ExecutionDependencies -> [Execution]
allExecutions edg = nub $ concatMap go $ edg
  where
    go (Nothing, e)  = [e]
    go (Just e1, e2) = [e1, e2]

graphToForest :: ExecutionDependencyGraph -> Forest Execution
graphToForest graph = map (buildBackwardsTreeFrom graph) $ depencencyRoots graph

buildBackwardsTreeFrom :: ExecutionDependencyGraph -> Execution -> Tree Execution
buildBackwardsTreeFrom graph v = Node {
        rootLabel = v
      , subForest = map (buildBackwardsTreeFrom graph) reachingV
    }
  where
    reachingV :: [Execution]
    reachingV = filter reachesV $ M.keys graph

    reachesV :: Execution -> Bool
    reachesV w = v `elem` (fromJust $ M.lookup w graph)

depencencyRoots :: ExecutionDependencyGraph -> [Execution]
depencencyRoots = M.keys . M.filter null

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
