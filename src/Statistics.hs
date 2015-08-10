module Statistics where

import           Solutions
import           Types

statistics :: EdenStatistics ()
statistics = runAllStats

runAllStats :: EdenStatistics ()
runAllStats = do
    intResults <- mapM runStat allIntStats
    liftIO $ putStrLn $ unlines $ map show intResults

allIntStats :: [StatGenerator Int]
allIntStats =
    [
        problemsStat
    ,   solutionsStat
    ,   languagesStat
    ]

problemsStat :: StatGenerator Int
problemsStat = StatGenerator {
        statgen_name = "Problems"
    ,   statgen_func = fmap length problems
    }

solutionsStat :: StatGenerator Int
solutionsStat = StatGenerator {
        statgen_name = "Solutions"
    ,   statgen_func = fmap length allSolutions
    }

languagesStat :: StatGenerator Int
languagesStat = StatGenerator {
        statgen_name = "Languages"
    ,   statgen_func = fmap length languages
    }

runStat :: StatGenerator a -> EdenStatistics (StatResult a)
runStat sg = do
    result <- statgen_func sg
    return StatResult {
            statres_name = statgen_name sg
        ,   statres_result = result
        }
