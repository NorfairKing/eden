module Paths where

import           Data.Char             (isSpace)
import           Data.Version          (showVersion)
import           Paths_eden            (version)
import           System.Directory      (getCurrentDirectory,
                                        getDirectoryContents)
import           System.Environment    (setEnv)
import           System.FilePath.Posix ((</>),
                                        takeDirectory)

import           Data.Maybe            (isJust)

import           Constants
import           Types

checkEden :: Eden c ()
checkEden = do
    mroot <- maybeEdenRoot
    case mroot of
        Nothing -> throwError "Not in an Eden repository."
        Just rt -> do
            checkVersion rt
            liftIO $ setEnv "EDEN_ROOT" rt

checkVersion :: FilePath -> Eden c ()
checkVersion root = do
    fileVersion <- liftIO $ readFile (versionPath root)
    if (trim fileVersion) == (showVersion version) then
        return ()
    else
        throwError "Repository version does not match binary version."

inEden :: Eden c Bool
inEden = fmap isJust maybeEdenRoot

edenRoot :: Eden c FilePath
edenRoot = do
    mroot <- maybeEdenRoot
    case mroot of
        Nothing -> throwError "Not in an Eden repository."
        Just fp -> return fp

dotEdenPath :: FilePath -> FilePath
dotEdenPath root = root </> dotEdenRootName

versionPath :: FilePath -> FilePath
versionPath root = (dotEdenPath root) </> versionFileName

maybeEdenRoot :: Eden c (Maybe FilePath)
maybeEdenRoot = do
    current <- liftIO $ getCurrentDirectory
    lookRecusivelyIn current
  where
    lookRecusivelyIn :: FilePath -> Eden c (Maybe FilePath)
    lookRecusivelyIn "/" = return Nothing
    lookRecusivelyIn dir = do
        contents <- liftIO $ getDirectoryContents dir
        if elem dotEdenRootName contents
        then return $ Just dir
        else lookRecusivelyIn $ takeDirectory dir

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
