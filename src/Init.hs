module Init where

import           System.Directory      (createDirectoryIfMissing,
                                        getCurrentDirectory)
import           System.FilePath.Posix ((</>))

import           Constants
import           Types

initE :: EdenInit ()
initE = do
    createEdenRoot

createEdenRoot :: EdenInit ()
createEdenRoot = do
    current <- liftIO getCurrentDirectory
    let edenRoot = current </> edenRootName
    liftIO $ createDirectoryIfMissing True edenRoot
