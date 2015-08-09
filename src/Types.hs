module Types
    (
      module Types
    , module Control.Monad.Except
    , module Control.Monad.IO.Class
    , module Control.Monad.Reader
    ) where

import           Control.Monad.Except   (ExceptT, catchError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT)



--[ Command line options ]--

data Options = Options {
        opt_global  :: GlobalOptions
    ,   opt_command :: Command
    } deriving Show

data GlobalOptions = GlobalOptions {
        opt_commands :: Bool
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

data GenerationTarget = Problem Problem
                      | Solution Problem Language
                      | Library Language
                      | Tests Language
                      | BuildDir Language
                      | Environment Language
                      | Publishing
                      | GettingStarted
  deriving Show

data BuildOptions = BuildOptions {
        build_target :: BuildTarget
    } deriving Show

data BuildTarget = BuildTarget {
        build_target_problem  :: Problem
    ,   build_target_language :: Language
    ,   build_target_makefile :: Maybe FilePath
    ,   build_target_makerule :: Maybe String
    } deriving Show

data TestOptions = TestOptions {
        test_target :: TestTarget
    } deriving Show

data TestTarget = TestTargetAll
                | TestTargetAllLibraries
                | TestTargetLibrary Language
                | TestTargetAllProblems
                | TestTargetProblem Problem
                | TestTargetSolution Problem Language
    deriving Show


data RunOptions = RunOptions {
        run_target :: RunTarget
    } deriving Show

data RunTarget = RunTarget {
        run_target_problem  :: Problem
    ,   run_target_language :: Language
    ,   run_target_input    :: Maybe FilePath
    ,   run_target_binary   :: String
    } deriving Show

data PublishOptions = PublishOptions {
        publish_target :: PublishTarget
    } deriving Show

data PublishTarget = PublishAll
                   | PublishProblem Problem
    deriving Show


--[ Monads ]--

type Eden c = ExceptT EdenError (ReaderT (GlobalOptions, c) IO)

type EdenError = String

type EdenInit       = Eden InitOptions
type EdenGenerate   = Eden GenerateOptions
type EdenBuild      = Eden BuildOptions
type EdenTest       = Eden TestOptions
type EdenRun        = Eden RunOptions
type EdenPublish    = Eden PublishOptions


--[ Euler Problems ]--

type Problem = Int
type Language = String
