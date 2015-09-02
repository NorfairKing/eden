module Statistics where

import           Text.PrettyPrint.Boxes

import           Data.List              (transpose)
import           Data.Maybe             (fromJust)

import           Solutions
import           Types

statistics :: EdenStatistics ()
statistics = runAllStats

runAllStats :: EdenStatistics ()
runAllStats = do
    table <- showSolutionStats
    liftIO $ putStrLn table
    intResults <- mapM runStat allIntStats
    liftIO $ putStrLn $ unlines $ map show intResults

showSolutionStats :: EdenStatistics String
showSolutionStats = do
    ls <- languages
    let titles = char ' ':(map text ls)
    table <- getTableRows ls
    let rows = titles:table
    let table = hsep 2 left (map (vcat center1) (transpose rows))
    return $ render $ table

getTableRows :: [Language] -> EdenStatistics [[Box]]
getTableRows ls = do
    ps <- problems
    mapM (getTableRow ls) ps

getTableRow :: [Language] -> Problem -> EdenStatistics [Box]
getTableRow ls p = do
    bs <- mapM (isSolvedIn p) ls
    return $ map text $ (show p: map showBool bs)
  where
    showBool True  = "[X]"
    showBool False = "[ ]"

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
