module Build where

import           Eden
import           Make
import           Paths
import           Solutions
import           Types
import           Utils



build :: Target -> EdenBuild ()
build TargetAll             = buildAll
build TargetAllLibraries    = buildLibraries
build (TargetLibrary l)     = buildLibrary l
build TargetAllProblems     = buildProblems
build (TargetProblem p)     = buildProblem p
build (TargetSolution p l)  = buildSolution p l

buildAll :: EdenBuild ()
buildAll = do
    buildLibraries
    buildProblems

buildLibraries :: EdenBuild ()
buildLibraries = do
    allLibraries <- libraries
    mapM_ buildLibrary allLibraries

buildLibrary :: Language -> EdenBuild ()
buildLibrary l = do
    md <- libraryDir l
    mf <- libMakefilePath l

    make md mf Nothing

buildSolution :: Problem -> Language -> EdenBuild ()
buildSolution p l = do
    build (TargetLibrary l) `catchError` (\e -> return ())

    md <- solutionDir p l
    bmf <- askEden build_makefile
    bmr <- askEden build_makerule
    mf <- case bmf of
            Nothing -> makefilePath l
            Just f  -> return f
    make md mf bmr

buildProblems :: EdenBuild ()
buildProblems = do
    allProblems <- problems
    mapM_ buildProblem allProblems

buildProblem :: Problem -> EdenBuild ()
buildProblem p = do
    allSolutions <- solutions p
    mapM_ (buildSolution p) allSolutions


--[ Building in general ]--

buildFirst :: EdenBuild a -> Eden c a
buildFirst builder = do
    o <- getGlobal
    (eea, mts) <- liftIO $ runEden builder (o, defaultBuildOption)
    case eea of
        Left err -> throwError err
        Right a  -> do
            makeTargetsFirst mts
            return a

defaultBuild :: EdenBuild a -> Eden c a
defaultBuild builder = do
    o <- getGlobal
    (eea, mts) <- liftIO $ runEden builder (o, defaultBuildOption)
    case eea of
        Left err -> throwError err
        Right a  -> do
            tell mts
            return a

defaultBuildOption :: BuildOptions
defaultBuildOption = BuildOptions {
        build_makefile = Nothing
      , build_makerule = Nothing
    }
