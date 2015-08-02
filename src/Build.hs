module Build where

import           Paths
import           Solutions
import           Types
import           Utils

build :: EdenBuild ()
build = do
    checkEden

    target <- askEden build_target
    buildTarget target

buildTarget :: BuildTarget -> EdenBuild ()
buildTarget bt = do
    md <- solutionDir (target_problem bt) (target_language bt)
    mf <- case target_makefile bt of
            Nothing -> makefilePath $ target_language bt
            Just f  -> return f
    let mr = target_makerule bt
    make md mf mr


make :: FilePath     -- Make directory
     -> FilePath     -- Make file
     -> Maybe String -- Make rule
     -> EdenBuild ()
make dir makefile mrule = do
    let rulestr = case mrule of
                    Nothing -> ""
                    Just rule -> rule
    let cmd = unwords $
            [
                "make"
            ,   "--directory"   , dir
            ,   "--file"        , makefile
            ,   rulestr
            ]
    liftIO $ runRaw cmd

