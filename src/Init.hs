module Init where


import           Data.Version          (showVersion)
import           System.Directory      (createDirectoryIfMissing,
                                        getCurrentDirectory)
import           Paths_eden            (version)
import           System.FilePath.Posix ((</>))
import           System.IO             (Handle,
                                        IOMode(WriteMode),
                                        hPutStrLn,
                                        withFile)

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
    let versionFile = edenRoot </> versionFileName
    liftIO $ withFile versionFile WriteMode createVersionFile

createVersionFile :: Handle -> IO ()
createVersionFile handle = hPutStrLn handle (showVersion version)
