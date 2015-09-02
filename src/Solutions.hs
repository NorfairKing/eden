module Solutions where

import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        getDirectoryContents)
import           System.FilePath.Posix ((</>))

import           Control.Monad         (filterM)
import           Data.List             (isPrefixOf, nub, sort, sortBy)
import           Data.Ord              (comparing)

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

checkProblem :: Problem -> Eden c ()
checkProblem p = do
    dir <- problemDir p
    exists <- liftIO $ doesDirectoryExist dir
    if exists
    then return ()
    else throwError $ unwords ["Problem", show p, "hasn't been solved."]

isSolvedIn :: Problem -> Language -> Eden c Bool
isSolvedIn p l = do
    dir <- solutionDir p l
    liftIO $ doesDirectoryExist dir

checkSolution :: Problem -> Language -> Eden c ()
checkSolution p l = do
    dir <- solutionDir p l
    solved <- p `isSolvedIn` l
    if solved
    then return ()
    else throwError $ unwords ["There is no solution in", l, "for problem", show p ++ "."]

checkLibrary :: Language -> Eden c ()
checkLibrary l = do
    dir <- libraryDir l
    exists <- liftIO $ doesDirectoryExist dir
    if exists
    then return ()
    else throwError $ unwords ["The library for", l, "doesn't exist."]

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

ioFilePaths :: Problem -> Eden c [(Maybe FilePath, FilePath)]
ioFilePaths p = do
    dir <- problemDir p
    cts <- liftIO $ getDirectoryContents dir

    let os = filter (isPrefixOf outputName) $ cts
    let is = map ((inputName ++) . drop (length outputName)) os
    let ifs = map (dir </>) is
    let ofs = map (dir </>) os
    tts <- mapM testCase $ zip ifs ofs
    return $ reverse . sortBy (comparing $ length . snd) $ tts

testCase :: (FilePath, FilePath) -> Eden c (Maybe FilePath, FilePath)
testCase (ip, op) = do
    exists <- liftIO $ doesFileExist ip
    return $ if exists
             then (Just ip, op)
             else (Nothing, op)

defaultIOFilePaths :: Problem -> Eden c (Maybe FilePath, FilePath)
defaultIOFilePaths p = do
    inp <- defaultInputFilePath p
    oup <- defaultOutputFilePath p
    return (Just inp, oup)

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
