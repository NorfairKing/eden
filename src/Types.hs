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

type Eden c = ReaderT (GlobalOptions, c) IO

type EdenGenerate   = Eden GenerateOptions
type EdenBuild      = Eden BuildOptions
type EdenTest       = Eden TestOptions
type EdenRun        = Eden RunOptions   
type EdenPublish    = Eden PublishOptions

runEden :: Eden c a -> (GlobalOptions, c) -> IO a
runEden = runReaderT

runEdenGenerator :: EdenGenerate a -> (GlobalOptions, GenerateOptions) -> IO a
runEdenGenerator = runEden

runEdenBuilder :: EdenBuild a -> (GlobalOptions, BuildOptions) -> IO a
runEdenBuilder = runEden

runEdenTester :: EdenTest a -> (GlobalOptions, TestOptions) -> IO a
runEdenTester = runEden

runEdenRunner :: EdenRun a -> (GlobalOptions, RunOptions) -> IO a
runEdenRunner = runEden

runEdenPublisher :: EdenPublish a -> (GlobalOptions, PublishOptions) -> IO a
runEdenPublisher = runEden


