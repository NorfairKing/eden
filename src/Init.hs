module Init where


import           Data.Version          (showVersion)
import           Paths_eden            (version)
import           System.Directory      (createDirectoryIfMissing,
                                        getCurrentDirectory)
import           System.FilePath.Posix ((</>))
import           System.IO             (Handle,
                                        IOMode(WriteMode),
                                        hPutStrLn,
                                        withFile)

import           Constants
import           Paths
import           Types

initE :: EdenInit ()
initE = do
    createEdenRoot

createEdenRoot :: EdenInit ()
createEdenRoot = do
    current <- liftIO getCurrentDirectory
    liftIO $ createDirectoryIfMissing True (dotEdenPath current)
    liftIO $ withFile (versionPath current) WriteMode createVersionFile

createVersionFile :: Handle -> IO ()
createVersionFile handle = hPutStrLn handle (showVersion version)
