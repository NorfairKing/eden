module Test where

import           Paths

import           Build
import           Constants
import           Eden
import           Solutions
import           Types
import           Utils

test :: EdenTest ()
test = do
    target <- askEden test_target
    testTarget target


testTarget :: TestTarget -> EdenTest ()
testTarget TestTargetAll = testAll
testTarget TestTargetAllLibraries = testLibraries
testTarget (TestTargetLibrary l) = testLibrary l
testTarget TestTargetAllProblems = testProblems
testTarget (TestTargetProblem p) = testProblem p
testTarget (TestTargetSolution p l) = testSolution p l

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
    buildLib l

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
    buildLib l
    buildTarget $ target p l

    md <- solutionDir p l
    mf <- makefilePath l
    let rule = Just defaultTestRuleName

    make md mf rule
