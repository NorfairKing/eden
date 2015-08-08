{-# LANGUAGE QuasiQuotes #-}
module Generate where

import           System.Directory          (createDirectoryIfMissing)
import           System.FilePath.Posix     ((</>))
import           System.IO                 (hFlush, stdout)
import           Text.Heredoc

import           Language.Haskell.TH.Quote

import           Eden
import           Paths
import           Solutions
import           TH
import           Types

generate :: EdenGenerate ()
generate = do
    target <- askEden generate_target
    case target of
        Problem p       -> generateProblem p
        Solution p l    -> generateSolution p l
        Library l       -> generateLibrary l
        Tests l         -> generateTests l
        BuildDir l      -> generateBuild l
        Environment l   -> generateEnvironment l
        Publishing      -> generatePublishing
        GettingStarted  -> generateGettingStarted


generateProblem :: Problem -> EdenGenerate ()
generateProblem p = do
    dir <- problemDir p
    liftIO $ createDirectoryIfMissing True dir


generateSolution :: Problem -> Language -> EdenGenerate ()
generateSolution p l = do
    dir <- solutionDir p l
    liftIO $ createDirectoryIfMissing True dir


generateLibrary :: Language -> EdenGenerate ()
generateLibrary l = do
    dir <- libraryDir l
    liftIO $ createDirectoryIfMissing True dir


generateTests :: Language -> EdenGenerate ()
generateTests l = do
    dir <- testsDir l
    liftIO $ createDirectoryIfMissing True dir


generateBuild :: Language -> EdenGenerate ()
generateBuild l = do
    dir <- buildFilesDir l
    liftIO $ createDirectoryIfMissing True dir


generateEnvironment :: Language -> EdenGenerate ()
generateEnvironment l = do
    generateLibrary l
    generateTests l
    generateBuild l

generatePublishing :: EdenGenerate ()
generatePublishing = do
    dir <- publishDir
    liftIO $ createDirectoryIfMissing True dir
    generateStarterPublishingFiles

generateStarterPublishingFiles :: EdenGenerate ()
generateStarterPublishingFiles = do
    dir <- publishDir
    liftIO $ writeFile starterMainTex $ dir </> "main.tex"

starterMainTex :: String
starterMainTex = [litFile|tex/main.tex|]

generateGettingStarted :: EdenGenerate ()
generateGettingStarted = do
    liftIO $ do
        putStrLn "What language would you like to use to solve the problems?"
        putStrLn "You can use more than one language later, but what will you start with?"
        putStrLn "Please use only lower case letters and no spaces."
        putStr "Language > "
        hFlush stdout
    lang <- liftIO $ getLine
    generateSolution 1 lang
    generateEnvironment lang
