module Solutions where

import           System.FilePath.Posix ((</>))

import           Constants
import           Paths
import           Types

--[ Problems ]--

problemDir :: Problem -> Eden c FilePath
problemDir p = do
    root <- edenRoot
    let dirName = problemDirName p
    return $ root </> dirName

problemDirName :: Problem -> FilePath
problemDirName = pad

--[ Solutions ]--

solutionDir :: Problem -> Language -> Eden c FilePath
solutionDir p l = do
    probDir <- problemDir p
    return $ probDir </> l


--[ Utils ]--

pad :: Int -> String
pad = padN paddingLength

padN :: Int -> Int -> String
padN m n = replicate (m - len) '0' ++ show n
  where len = length $ show n

