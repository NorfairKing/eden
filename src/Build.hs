module Build where

import           Eden
import           Make
import           Schedule
import           Solutions
import           Types

build :: Target -> EdenBuild ()
build TargetAll             = buildAll
build TargetAllLibraries    = buildLibraries
build (TargetLibrary l)     = buildSingleLibrary l
build TargetAllProblems     = buildProblems
build (TargetProblem p)     = buildProblem p
build (TargetSolution p l)  = buildSingleSolution p l

buildAll :: EdenBuild ()
buildAll = do
    buildLibraries
    buildProblems

buildLibraries :: EdenBuild ()
buildLibraries = do
    allLibraries <- libraries
    mapM_ buildSingleLibrary allLibraries

buildSingleLibrary :: Language -> EdenBuild ()
buildSingleLibrary l = buildLibrary l >>= schedule

buildLibrary :: Language -> Eden c [Execution]
buildLibrary l = do
    md <- libraryDir l
    mf <- libMakefilePath l
    return $ [make md mf Nothing]

buildProblems :: EdenBuild ()
buildProblems = do
    allProblems <- problems
    mapM_ buildProblem allProblems

buildProblem :: Problem -> EdenBuild ()
buildProblem p = do
    allSolutions <- solutions p
    mapM_ (buildSingleSolution p) allSolutions

buildSingleSolution :: Problem -> Language -> EdenBuild ()
buildSingleSolution p l = do
    bmf <- askEden build_makefile
    bmr <- askEden build_makerule
    buildSolution p l bmf bmr >>= schedule

buildSolution :: Problem
              -> Language
              -> Maybe FilePath -- makefile
              -> Maybe String -- make rule
              -> Eden c [Execution]
buildSolution p l bmf bmr = do
    btl <- buildLibrary l

    md <- solutionDir p l
    mf <- case bmf of
            Nothing -> makefilePath l
            Just f  -> return f
    let mt = make md mf bmr
    return $ btl ++ [mt]
