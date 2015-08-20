module Solutions where

import           System.Directory      (doesFileExist, getDirectoryContents)
import           System.FilePath.Posix ((</>))

import           Control.Monad         (filterM)
import           Data.List             (nub, sort)

import           Constants
import           Paths
import           Types
import           Utils

--[ Problems ]--

problemDir :: Problem -> Eden c FilePath
problemDir p = do
    root <- edenRoot
    let dirName = problemDirName p
    return $ root </> dirName

problemDirName :: Problem -> FilePath
problemDirName = padNWith 3 '0' . show

problems :: Eden c [Problem]
problems = do
    root <- edenRoot
    cts <- liftIO $ getDirectoryContents root
    return $ sort $ map read $ filter isProblemDir cts
  where
    isProblemDir d = elem d $ map problemDirName nums
    nums = [1..999]


--[ Solutions ]--

solutionDir :: Problem -> Language -> Eden c FilePath
solutionDir p l = do
    probDir <- problemDir p
    return $ probDir </> l

solutions :: Problem -> Eden c [FilePath]
solutions p = do
    dir <- problemDir p
    cts <- liftIO $ getDirectoryContents dir
    return $ sort $ filter realDir cts

allSolutions :: Eden c [FilePath]
allSolutions = do
    probs <- problems
    allSols <- mapM solutions probs
    return $ concat allSols

languages :: Eden c [Language]
languages = do
    allSols <- allSolutions
    return $ nub allSols

actualSolutionInput :: Problem -> Maybe FilePath -> Eden c (Maybe FilePath)
actualSolutionInput p inp = case inp of
                            Just rti -> return $ Just rti
                            Nothing  -> do
                                dif <- defaultInputFilePath p
                                exists <- liftIO $ doesFileExist dif
                                if exists
                                then return $ Just dif
                                else return $ Nothing

--[ Libraries ]--

libDir :: Eden c FilePath
libDir = do
    root <- edenRoot
    return $ root </> libDirName

libraryDir :: Language -> Eden c FilePath
libraryDir l = do
    dir <- libDir
    return $ dir </> l

libMakefilePath :: Language -> Eden c FilePath
libMakefilePath l = do
    dir <- libraryDir l
    return $ dir </> defaultMakefileName

libraries :: Eden c [Language]
libraries = do
    dir <- libDir
    cts <- liftIO $ getDirectoryContents dir
    return $ filter realDir cts

--[ Tests ]--

testDir :: Eden c FilePath
testDir = do
    root <- edenRoot
    return $ root </> testDirName

testsDir :: Language -> Eden c FilePath
testsDir l = do
    dir <- testDir
    return $ dir </> l

testMakefilePath :: Language -> Eden c FilePath
testMakefilePath l = do
    dir <- testsDir l
    return $ dir </> defaultMakefileName

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


--[ Run ]--

defaultSolutionBinary :: Problem -> Language -> Eden c FilePath
defaultSolutionBinary p l = do
    dir <- solutionDir p l
    return $ dir </> defaultExecutableName

defaultInputFilePath :: Problem -> Eden c FilePath
defaultInputFilePath p = do
    dir <- problemDir p
    return $ dir </> defaultInputFileName

defaultOutputFilePath :: Problem -> Eden c FilePath
defaultOutputFilePath p = do
    dir <- problemDir p
    return $ dir </> defaultOutputFileName


--[ Publish ]--

publishDir :: Eden c FilePath
publishDir = do
    root <- edenRoot
    return $ root </> publishingDirName

explanationPath :: Problem -> Eden c FilePath
explanationPath p = do
    probDir <- problemDir p
    return $ probDir </> defaultExplanationName

explanations :: Eden c [Problem]
explanations = do
    pbs <- problems
    filterM containsExplanation pbs
  where
    containsExplanation :: Problem -> Eden c Bool
    containsExplanation p = do
        probDir <- problemDir p
        liftIO $ doesFileExist $ probDir </> defaultExplanationName
