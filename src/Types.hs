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

data Command = Init InitOptions
             | Generate GenerateOptions
             | Build BuildOptions
             | Test TestOptions
             | Run RunOptions
             | Publish PublishOptions
  deriving Show

data InitOptions = InitOptions
  deriving Show

data GenerateOptions = GenerateOptions {
        generate_target :: GenerationTarget
    }
  deriving Show

data GenerationTarget = Problem Int
                      | Solution Int Language
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

type EdenInit       = Eden InitOptions
type EdenGenerate   = Eden GenerateOptions
type EdenBuild      = Eden BuildOptions
type EdenTest       = Eden TestOptions
type EdenRun        = Eden RunOptions
type EdenPublish    = Eden PublishOptions

runEden :: Eden c a -> (GlobalOptions, c) -> IO a
runEden = runReaderT

runEdenInitialiser :: EdenInit a -> (GlobalOptions, InitOptions) -> IO a
runEdenInitialiser = runEden

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

askEden :: (c -> a) -> Eden c a
askEden func = do
    (_, o) <- ask
    return $ func o


--[ Euler Problems ]--

type Problem = Int
type Language = String

type Solution = (Problem, Language)


