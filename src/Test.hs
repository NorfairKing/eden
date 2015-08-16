module Test where

import           Build
import           Constants
import           Make
import           Solutions
import           Types

test :: Target -> EdenTest ()
test TargetAll = testAll
test TargetAllLibraries = testLibraries
test (TargetLibrary l) = testLibrary l
test TargetAllProblems = testProblems
test (TargetProblem p) = testProblem p
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
    defaultBuild $ buildLib l

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
    defaultBuild $ buildLib l
    defaultBuild $ build $ TargetSolution p l

    md <- solutionDir p l
    mf <- makefilePath l
    let rule = Just defaultTestRuleName

    make md mf rule
