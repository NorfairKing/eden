module Eden where

import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.Reader   (ReaderT, ask, asks, runReaderT)

import           Paths
import           Types

runCheckedEden :: Eden c a -> (GlobalOptions, c) -> IO (Either EdenError a)
runCheckedEden func = runEden (checkEden >> func)

runEden :: Eden c a -> (GlobalOptions, c) -> IO (Either EdenError a)
runEden func = runReaderT (runExceptT func)

askEden :: (c -> a) -> Eden c a
askEden func = do
    (_, o) <- ask
    return $ func o

askGlobal :: (GlobalOptions -> a) -> Eden c a
askGlobal func = do
    (o, _) <- ask
    return $ func o
