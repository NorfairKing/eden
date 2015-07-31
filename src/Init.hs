module Init where

import           Control.Exception     (try)
import           System.Directory      (createDirectory, getCurrentDirectory)
import           System.FilePath.Posix (replaceBaseName, (</>))

import           Types

initE :: EdenInit ()
initE = do
    createEdenRoot

edenRootName :: String
edenRootName = ".eden"

createEdenRoot :: EdenInit ()
createEdenRoot = do
    current <- liftIO getCurrentDirectory
    let edenRoot = current </> edenRootName
    liftIO $ createDirectory edenRoot
