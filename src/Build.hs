module Build where


import           Paths
import           Types
import           Utils

build :: EdenBuild ()
build = do
    checkEden

    target <- askEden build_target
    buildTarget target

buildTarget :: BuildTarget -> EdenBuild ()
buildTarget bt = do
    liftIO $ print bt
