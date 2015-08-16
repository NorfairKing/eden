module Make where

import           System.Directory (doesDirectoryExist, doesFileExist)

import           Eden
import           Types
import           Utils


make :: FilePath     -- Make directory
     -> FilePath     -- Make file
     -> Maybe String -- Make rule
     -> Eden c ()
make dir mf mr = do
    let mt = MakeTarget {
            make_dir  = dir
          , make_file = mf
          , make_rule = mr
        }
    tell $ singleMakeTarget mt

singleMakeTarget :: MakeTarget -> MakeTargets
singleMakeTarget mt = MakeTargets { make_targets = [mt] }

makeTargetsFirst :: MakeTargets -> Eden c ()
makeTargetsFirst mts = do
    o <- getGlobal
    liftIO $ runEdenMake (makeTargets mts) o

makeTargets :: MakeTargets -> EdenMake ()
makeTargets mts = mapM_ doMake $ make_targets mts

doMake :: MakeTarget -> EdenMake ()
doMake mt = do
    let dir = make_dir mt
        makefile = make_file mt
        mrule = make_rule mt
    dirExists   <- liftIO $ doesDirectoryExist dir
    fileExists <- liftIO $ doesFileExist makefile

    if not dirExists
    then throwError $ unwords ["Directory", dir, "does not exist."]
    else do
        if not fileExists
        then throwError $ unwords ["Makefile", makefile, "does not exist."]
        else do
            let rulestr = case mrule of
                        Nothing -> ""
                        Just rule -> rule
            let cmd = unwords $
                    [
                        "make"
                    ,   "--directory"   , dir
                    ,   "--file"        , makefile
                    ,   "--jobs"
                    ,   rulestr
                    ]
            runRaw cmd

