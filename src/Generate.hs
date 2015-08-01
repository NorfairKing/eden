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
        Problem p -> generateProblem p
        Solution p l -> generateSolution p l


generateProblem :: Problem -> EdenGenerate ()
generateProblem p = do
    dir <- problemDir p
    liftIO $ createDirectoryIfMissing True dir

generateSolution :: Problem -> Language -> EdenGenerate ()
generateSolution p l = do
    dir <- solutionDir p l
    liftIO $ createDirectoryIfMissing True dir

