module Eden where

import           Paths
import           Types

runCheckedEden :: Eden c a -> (GlobalOptions, c) -> IO (Either EdenError a, ExecutionDependencies)
runCheckedEden func = runEden (checkEden >> func)

runEden :: Eden c a -> (GlobalOptions, c) -> IO (Either EdenError a, ExecutionDependencies)
runEden func = runReaderT (runWriterT (runExceptT func))

runEdenMake :: EdenMake a -> GlobalOptions -> IO a
runEdenMake func o = do
    (ee, _) <- runEden func (o, ())
    case ee of
        Left err -> error err
        Right a  -> return a

getOptions :: Eden c c
getOptions = fmap snd ask

askEden :: (c -> a) -> Eden c a
askEden func = fmap func getOptions

getGlobal :: Eden c GlobalOptions
getGlobal = fmap fst ask

askGlobal :: (GlobalOptions -> a) -> Eden c a
askGlobal func = fmap func getGlobal
