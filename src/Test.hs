module Test where

import           Build
import           Constants
import           Make
import           Run
import           Solutions
import           Types
import           Utils

test :: Target -> EdenTest ()
test TargetAll            = testAll
test TargetAllLibraries   = testLibraries
test (TargetLibrary l)    = testLibrary l
test TargetAllProblems    = testProblems
test (TargetProblem p)    = testProblem p
test (TargetSolution p l) = testSolution p l

testAll :: EdenTest ()
testAll = do
    testLibraries
    testProblems

testLibraries :: EdenTest ()
testLibraries = do
    allLibraries <- libraries
    mapM_ testLibrary allLibraries

testLibrary :: Language -> EdenTest ()
testLibrary l = do
    defaultBuild $ buildLibrary l

    md <- testsDir l
    mf <- testMakefilePath l
    let rule = Just defaultTestRuleName

    make md mf rule

testProblems :: EdenTest ()
testProblems = do
    allProblems <- problems
    mapM_ testProblem allProblems

testProblem :: Problem -> EdenTest ()
testProblem p = do
    allSolutions <- solutions p
    mapM_ (testSolution p) allSolutions

testSolution :: Problem -> Language -> EdenTest ()
testSolution p l = do
    buildFirst $ buildLibrary l
    buildFirst $ build $ TargetSolution p l

    md <- solutionDir p l
    mf <- makefilePath l
    let rule = Just defaultTestRuleName

    make md mf rule

    actual <- defaultRun (runSolution p l)
    dof <- defaultOutputFilePath p
    expected <- readFromFile dof

    let same = ["Test:", problemDirName p, padNWith 8 ' ' l ++ ":"]
    if actual /= expected
    then          throwError $ unwords $ same ++ ["Fail,", "Expected:", show expected, "Actual:", show actual]
    else liftIO $ putStrLn   $ unwords $ same ++ ["Success."]


