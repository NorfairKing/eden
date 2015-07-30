module Types
    (
      module Types
    , module Control.Monad.Reader
    , module Control.Monad.IO.Class
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, asks, runReaderT)


--[ Command line options ]--

data Options = Options {
        opt_global  :: GlobalOptions
    ,   opt_command :: Command
    } deriving Show

data GlobalOptions = GlobalOptions {
        opt_quiet :: Bool
    } deriving Show

data Command = Generate GenerateOptions
             | Build BuildOptions
             | Test TestOptions
             | Run RunOptions
             | Publish PublishOptions
  deriving Show

data GenerateOptions = GenerateOptions
  deriving Show

data BuildOptions = BuildOptions
  deriving Show

data TestOptions = TestOptions
  deriving Show

data RunOptions = RunOptions
  deriving Show

data PublishOptions = PublishOptions
  deriving Show



--[ Monads ]--

type EdenGenerate   = ReaderT (GlobalOptions, GenerateOptions)  IO
type EdenBuild      = ReaderT (GlobalOptions, BuildOptions)     IO
type EdenTest       = ReaderT (GlobalOptions, TestOptions)      IO
type EdenRun        = ReaderT (GlobalOptions, RunOptions)       IO
type EdenPublish    = ReaderT (GlobalOptions, PublishOptions)   IO

runEdenGenerator :: EdenGenerate a -> (GlobalOptions, GenerateOptions) -> IO a
runEdenGenerator = runReaderT

runEdenBuilder :: EdenBuild a -> (GlobalOptions, BuildOptions) -> IO a
runEdenBuilder = runReaderT

runEdenTester :: EdenTest a -> (GlobalOptions, TestOptions) -> IO a
runEdenTester = runReaderT

runEdenRunner :: EdenRun a -> (GlobalOptions, RunOptions) -> IO a
runEdenRunner = runReaderT

runEdenPublisher :: EdenPublish a -> (GlobalOptions, PublishOptions) -> IO a
runEdenPublisher = runReaderT
