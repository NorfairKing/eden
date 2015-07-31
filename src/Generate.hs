module Generate where

import Types

generate :: EdenGenerate ()
generate = do
    target <- askEden generate_target
    case target of
        Problem p -> generateProblem p
        Solution p l -> generateSolution p l
        

generateProblem :: Problem -> EdenGenerate ()
generateProblem p = liftIO $ print p

generateSolution :: Problem -> Language -> EdenGenerate ()
generateSolution p l = liftIO $ print (p,l)
