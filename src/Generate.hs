module Generate where

import           System.Directory (createDirectoryIfMissing)

import           Paths
import           Solutions
import           Types

generate :: EdenGenerate ()
generate = do
    checkEden

    target <- askEden generate_target
    case target of
        Problem p       -> generateProblem p
        Solution p l    -> generateSolution p l
        Library l       -> generateLibrary l
        Tests l         -> generateTests l
        Environment l   -> generateEnvironment l


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


generateEnvironment :: Language -> EdenGenerate ()
generateEnvironment l = do
    generateLibrary l
    generateTests l



