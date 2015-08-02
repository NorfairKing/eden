module Solutions where

import           System.FilePath.Posix ((</>))

import           Constants
import           Paths
import           Types

--[ Language ]--




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


--[ Libraries ]--

libDir :: Eden c FilePath
libDir = do
    root <- edenRoot
    return $ root </> libDirName

libraryDir :: Language -> Eden c FilePath
libraryDir l = do
    dir <- libDir
    return $ dir </> l


--[ Tests ]--

testDir :: Eden c FilePath
testDir = do
    root <- edenRoot
    return $ root </> testDirName

testsDir :: Language -> Eden c FilePath
testsDir l = do
    dir <- testDir
    return $ dir </> l

--[ Builds ]--

buildDir :: Eden c FilePath
buildDir = do
    root <- edenRoot
    return $ root </> buildDirName

buildFilesDir :: Language -> Eden c FilePath
buildFilesDir l = do
    dir <- buildDir
    return $ dir </> l

makefilePath :: Language -> Eden c FilePath
makefilePath l = do
    dir <- buildFilesDir l
    return $ dir </> defaultMakefileName



--[ Utils ]--

pad :: Int -> String
pad = padN paddingLength

padN :: Int -> Int -> String
padN m n = replicate (m - len) '0' ++ show n
  where len = length $ show n
