module Build where

import           Paths
import           Solutions
import           Types
import           Utils

build :: EdenBuild ()
build = do
    checkEden

    target <- askEden build_target

    buildLib $ build_target_language target
    buildTarget target

buildLib :: Language -> EdenBuild ()
buildLib l = do
    md <- libraryDir l
    mf <- libMakefilePath l
    make md mf Nothing

buildTarget :: BuildTarget -> EdenBuild ()
buildTarget bt = do
    md <- solutionDir (build_target_problem bt) (build_target_language bt)
    mf <- case build_target_makefile bt of
            Nothing -> makefilePath $ build_target_language bt
            Just f  -> return f
    let mr = build_target_makerule bt
    make md mf mr
