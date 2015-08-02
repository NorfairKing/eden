module Paths where

import           System.Directory      (getCurrentDirectory,
                                        getDirectoryContents)
import           System.Environment    (setEnv)
import           System.FilePath.Posix (takeDirectory)

import           Data.Maybe            (isJust)

import           Constants
import           Types

checkEden :: Eden c ()
checkEden = do
    mroot <- maybeEdenRoot
    case mroot of
        Nothing -> throwError "Not in an Eden repository."
        Just rt -> liftIO $ setEnv "EDEN_ROOT" rt

inEden :: Eden c Bool
inEden = fmap isJust maybeEdenRoot

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
