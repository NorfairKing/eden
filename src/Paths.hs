module Paths where

import           System.Directory      (getCurrentDirectory,
                                        getDirectoryContents)
import           System.FilePath.Posix (takeDirectory, (</>))

import           Constants
import           Types

edenRoot :: Eden c FilePath
edenRoot = do
    mroot <- maybeEdenRoot
    case mroot of
        Nothing -> throwError "Not in an Eden repository."
        Just fp -> return fp

maybeEdenRoot :: Eden c (Maybe FilePath)
maybeEdenRoot = do
    current <- liftIO $ getCurrentDirectory
    lookRecusivelyIn current
  where
    lookRecusivelyIn :: FilePath -> Eden c (Maybe FilePath)
    lookRecusivelyIn "/" = return Nothing
    lookRecusivelyIn dir = do
        contents <- liftIO $ getDirectoryContents dir
        if elem edenRootName contents
        then return $ Just dir
        else lookRecusivelyIn $ takeDirectory dir
