module Eden where

import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import           Control.Monad.Writer (WriterT, runWriterT, tell)

import           Paths
import           Types

runCheckedEden :: Eden c a -> (GlobalOptions, c) -> IO (Either EdenError a, MakeTargets)
runCheckedEden func = runEden (checkEden >> func)

runEden :: Eden c a -> (GlobalOptions, c) -> IO (Either EdenError a, MakeTargets)
runEden func = runReaderT (runWriterT (runExceptT func))

getOptions :: Eden c c
getOptions = fmap snd ask

askEden :: (c -> a) -> Eden c a
askEden func = fmap func getOptions

getGlobal :: Eden c GlobalOptions
getGlobal = fmap fst ask

askGlobal :: (GlobalOptions -> a) -> Eden c a
askGlobal func = fmap func getGlobal
