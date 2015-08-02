module Test where

import           Paths

import           Build
import           Constants
import           Solutions
import           Types
import           Utils

test :: EdenTest ()
test = do
    checkEden

    target <- askEden test_target
    testTarget target


testTarget :: TestTarget -> EdenTest ()
testTarget TestTargetAll = testAll
testTarget (TestTargetLibrary l) = testLibrary l
testTarget (TestTargetProblem p) = testProblem p
testTarget (TestTargetSolution p l) = testSolution p l

testAll :: EdenTest ()
testAll = undefined

testLibrary :: Language -> EdenTest ()
testLibrary l = undefined

testProblem :: Problem -> EdenTest ()
testProblem p = undefined

testSolution :: Problem -> Language -> EdenTest ()
testSolution p l = do
    buildLib l
    buildTarget $ target p l

    md <- solutionDir p l
    mf <- makefilePath l
    let rule = Just defaultTestRuleName

    make md mf rule
