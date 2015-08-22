module Test where

import           Build
import           Constants
import           Make
import           Schedule
import           Solutions
import           Types

test :: Target -> EdenTest ()
test TargetAll            = testAll
test TargetAllLibraries   = testLibraries
test (TargetLibrary l)    = testSingleLibrary l
test TargetAllProblems    = testProblems
test (TargetProblem p)    = testProblem p
test (TargetSolution p l) = testSingleSolution p l

testAll :: EdenTest ()
testAll = do
    testLibraries
    testProblems

testLibraries :: EdenTest ()
testLibraries = do
    allLibraries <- libraries
    mapM_ testSingleLibrary allLibraries

testSingleLibrary :: Language -> EdenTest ()
testSingleLibrary l = testLibrary l >>= schedule

testLibrary :: Language -> Eden c [Execution]
testLibrary l = do
    blt <- buildLibrary l

    md <- testsDir l
    mf <- testMakefilePath l
    let rule = Just defaultTestRuleName
    let mt =  make md mf rule

    return $ blt ++ [mt]

testProblems :: EdenTest ()
testProblems = do
    allProblems <- problems
    mapM_ testProblem allProblems

testProblem :: Problem -> EdenTest ()
testProblem p = do
    allSolutions <- solutions p
    mapM_ (testSingleSolution p) allSolutions

testSingleSolution :: Problem -> Language -> EdenTest ()
testSingleSolution p l = testSolution p l >>= schedule

testSolution :: Problem -> Language -> Eden c [Execution]
testSolution p l = do
    bts <- buildSolution p l Nothing Nothing

    md <- solutionDir p l
    mf <- makefilePath l
    let rule = Just defaultTestRuleName
    let btm = make md mf rule

    bin <- defaultSolutionBinary p l
    iops <- ioFilePaths p
    let tts = map (testTarget p l bin) iops
    return $ bts ++ [btm] ++ tts

testTarget :: Problem -> Language -> FilePath -> (Maybe FilePath, FilePath) -> Execution
testTarget p l bin (mip, op) =
    TestRunExecution TestTarget {
        test_target_problem  = p
      , test_target_language = l
      , test_target_bin      = bin
      , test_target_input    = mip
      , test_target_output   = op
      }
