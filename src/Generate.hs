{-# LANGUAGE QuasiQuotes #-}
module Generate where

import           System.Directory      (createDirectoryIfMissing)
import           System.FilePath.Posix ((</>))
import           System.IO             (hFlush, stdout)

import           Constants
import           Solutions
import           TH
import           Types

generate :: GenerationTarget -> EdenGenerate ()
generate (Problem p)      = generateProblem p
generate (Solution p l)   = generateSolution p l
generate (Library l)      = generateLibrary l
generate (Tests l)        = generateTests l
generate (BuildDir l)     = generateBuild l
generate (Environment l)  = generateEnvironment l
generate Publishing       = generatePublishing
generate (GettingStarted) = generateGettingStarted


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
    case starterMakefile l of
        Nothing -> return ()
        Just ss -> liftIO $ writeFile (dir </> defaultMakefileName) ss

cMakefile :: String
cMakefile = [litFile|starter-makefiles/c|]

haskellMakefile :: String
haskellMakefile = [litFile|starter-makefiles/haskell|]

pythonMakefile :: String
pythonMakefile = [litFile|starter-makefiles/python|]

javaMakefile :: String
javaMakefile = [litFile|starter-makefiles/java|]

starterMakefile :: Language -> Maybe String
starterMakefile lang = lookup lang $
    [
        ("c", cMakefile)
    ,   ("haskell", haskellMakefile)
    ,   ("python", pythonMakefile)
    ,   ("java", javaMakefile)
    ]

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
    liftIO $ writeFile (dir </> "main.tex") starterMainTex

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
