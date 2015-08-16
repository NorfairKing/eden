module Build where

import           Eden
import           Make
import           Paths
import           Solutions
import           Types
import           Utils



build :: Target -> EdenBuild ()
build TargetAll = notImplementedYet
build TargetAllLibraries = notImplementedYet
build (TargetLibrary l) = buildLib l
build TargetAllProblems = notImplementedYet
build (TargetProblem p) = notImplementedYet
build (TargetSolution p l) = buildSolution p l

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

buildLib :: Language -> EdenBuild ()
buildLib l = do
    md <- libraryDir l
    mf <- libMakefilePath l

    make md mf Nothing

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
