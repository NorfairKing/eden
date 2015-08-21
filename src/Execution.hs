module Execution where

import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           System.Directory         (doesDirectoryExist, doesFileExist)

import           Data.Graph               (Graph, Vertex, dfs, graphFromEdges',
                                           vertices)
import           Data.List                (nub)
import           Data.Map                 (Map, fromList, lookup, toList,
                                           update)
import           Data.Maybe               (fromJust)
import           Data.Tree                (Forest, Tree (..), levels)
import           Prelude                  hiding (lookup)

import           Eden
import           Solutions
import           Types
import           Utils

executeGraph :: ExecutionDependencyGraph -> EdenMake ()
executeGraph ef = do
    liftIO $ B.putStrLn $ encodePretty ef
    let forest = toForest ef
    liftIO $ B.putStrLn $ encodePretty forest
    executeForest forest

executeForest :: ExecutionForest -> EdenMake ()
executeForest = mapM_ executeTree

executeTree :: ExecutionTree -> EdenMake ()
executeTree et = do
    execute $ rootLabel et
    executeForest $ subForest et

execute :: Execution -> EdenMake ()
execute (MakeExecution mt)    = doMake mt
execute (RunExecution rt)     = doRunExecution rt >> return ()
execute (TestRunExecution rt) = doTestExecution rt

toForest :: ExecutionDependencyGraph -> ExecutionForest
toForest edg = map (fmap mapBack) $ graphToForest
  where
    graphToForest :: Forest Vertex
    graphToForest = map buildBackwardsTreeFromVertex roots
      where roots = filter (\v -> null . thd3 $ vertexLookupFunction v) $ vertices graph

    buildBackwardsTreeFromVertex :: Vertex -> Tree Vertex
    buildBackwardsTreeFromVertex v = Node {
            rootLabel = v
          , subForest = map buildBackwardsTreeFromVertex $ reachingV
        }
      where
        reachingV :: [Vertex]
        reachingV = filter reachesV $ vertices graph

        reachesV :: Vertex -> Bool
        reachesV w = v `elem` (thd3 $ vertexLookupFunction w)

    mapBack :: Vertex -> Execution
    mapBack e = fst3 $ vertexLookupFunction e

    graph :: Graph
    graph = fst $ graphFromEdges' realGraph

    vertexLookupFunction :: Vertex -> (Execution, Int, [Int])
    vertexLookupFunction = snd $ graphFromEdges' realGraph

    backwardsMap :: Map Int Execution
    backwardsMap = fromList $ zip [1..] $ map fst dependencyList

    realGraph :: [(Execution, Int, [Vertex])]
    realGraph = toRealEdges dependencyList

    toRealEdges :: [(Execution, [Execution])] -> [(Execution, Int, [Int])]
    toRealEdges egs = map go egs
      where
        go :: (Execution, [Execution]) -> (Execution, Int, [Int])
        go (e, es) = (e, lo e, map lo es)

        lo :: Execution -> Int
        lo = fromJust . (flip lookup) executionsVertexMap

        executionsVertexMap :: Map Execution Int
        executionsVertexMap = fromList $ zip (map fst dependencyList) [1..]

    dependencyList :: [(Execution, [Execution])]
    dependencyList = toList dependencyMap

    dependencyMap :: Map Execution [Execution]
    dependencyMap = addAll edg startingMap

    startingMap :: Map Execution [Execution]
    startingMap = fromList $ zip allExecutions $ repeat []

    -- Map to dependencies: (a -> b) means b has to happen before a
    addAll :: ExecutionDependencyGraph -> Map Execution [Execution] -> Map Execution [Execution]
    addAll [] accMap                  = accMap
    addAll ((Nothing, _): es) accMap  = addAll es accMap
    addAll ((Just bf, af):es) accMap  = addAll es $ update fn af accMap
      where
        fn :: [Execution] -> Maybe [Execution]
        fn es = Just (bf:es)

    allExecutions :: [Execution]
    allExecutions = nub $ concatMap go $ edg
      where
        go (Nothing, e)  = [e]
        go (Just e1, e2) = [e1, e2]



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

