module Init where

import           System.Directory      (createDirectory, getCurrentDirectory)
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
    liftIO $ createDirectory edenRoot
