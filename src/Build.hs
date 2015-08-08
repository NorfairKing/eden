module Build where

import           System.Directory (doesDirectoryExist, doesFileExist)

import           Eden
import           Paths
import           Solutions
import           Types
import           Utils

build :: EdenBuild ()
build = do
    target <- askEden build_target

    buildLib $ build_target_language target
    buildTarget target

buildLib :: Language -> Eden c ()
buildLib l = do
    md <- libraryDir l
    mf <- libMakefilePath l

    libdirExists   <- liftIO $ doesDirectoryExist md
    makefileExists <- liftIO $ doesFileExist mf

    if libdirExists && makefileExists
    then make md mf Nothing
    else return ()


buildTarget :: BuildTarget -> Eden c ()
buildTarget bt = do
    md <- solutionDir (build_target_problem bt) (build_target_language bt)
    mf <- case build_target_makefile bt of
            Nothing -> makefilePath $ build_target_language bt
            Just f  -> return f
    let mr = build_target_makerule bt
    make md mf mr

target :: Problem -> Language -> BuildTarget
target p l = BuildTarget {
        build_target_problem = p
    ,   build_target_language = l
    ,   build_target_makefile = Nothing
    ,   build_target_makerule = Nothing
    }
