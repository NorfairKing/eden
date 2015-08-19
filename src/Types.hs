module Types
    ( module Types
    , module Control.Monad.Except
    , module Control.Monad.IO.Class
    , module Control.Monad.Reader
    , module Control.Monad.Writer
    ) where

import           Control.Monad.Except   (ExceptT, MonadError, catchError,
                                         runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT, ask, asks, runReaderT)
import           Control.Monad.Writer   (WriterT, runWriterT, tell)

import           Data.List              (nub)

--[ Command line options ]--

data Options = Options {
        opt_global  :: GlobalOptions
    ,   opt_command :: Command
    } deriving Show

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions = GlobalOptions {
        opt_commands = False
    }
data GlobalOptions = GlobalOptions {
        opt_commands :: Bool
    } deriving Show

data Command = Init InitOptions
             | Generate GenerateOptions GenerationTarget
             | Build BuildOptions Target
             | Test TestOptions Target
             | Run RunOptions Target
             | Publish PublishOptions PublishTarget
             | Statistics StatisticsOptions
  deriving Show

data InitOptions = InitOptions
  deriving Show

data GenerateOptions = GenerateOptions
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
        build_makefile :: Maybe FilePath
    ,   build_makerule :: Maybe String
    } deriving Show

data TestOptions = TestOptions
    deriving Show

data RunOptions = RunOptions {
        run_input  :: Maybe FilePath
    ,   run_binary :: String
    } deriving Show

data PublishOptions = PublishOptions
    deriving Show

data PublishTarget = PublishAll
                   | PublishLibrary FilePath
                   | PublishPart FilePath
                   | PublishProblem Problem
    deriving Show

data StatisticsOptions = StatisticsOptions
    deriving Show


--[ Statistics ]--

data StatGenerator a = StatGenerator {
        statgen_name :: String
    ,   statgen_func :: EdenStatistics a
    }

data StatResult a = StatResult {
        statres_name   :: String
    ,   statres_result :: a
    }

instance Show a => Show (StatResult a) where
    show sr = statres_name sr ++ ": " ++ show (statres_result sr)


--[ Targets ]--

data Target = TargetAll
            | TargetAllLibraries
            | TargetLibrary Language
            | TargetAllProblems
            | TargetProblem Problem
            | TargetSolution Problem Language
    deriving Show

data ExecutionForest = ExecutionForest {
        execution_targets :: [ExecutionTarget]
    } deriving (Show, Eq)

instance Monoid ExecutionForest where
    mempty  = ExecutionForest { execution_targets = [] }
    mappend mt1 mt2 = ExecutionForest {
            execution_targets = nub $ execution_targets mt1 ++ execution_targets mt2
        }

data ExecutionTarget = ExecutionTarget {
      execution            :: Execution
    , execution_dependants :: [ExecutionTarget]
  } deriving (Show, Eq)

data Execution = MakeExecution MakeTarget
               | RunExecution RunTarget
               | TestRunExecution TestTarget
  deriving (Show, Eq)

data MakeTarget = MakeTarget {
      make_dir  :: FilePath
    , make_file :: FilePath
    , make_rule :: Maybe String
  } deriving (Show, Eq)

data TestTarget = TestTarget {
      test_target_problem  :: Problem
    , test_target_language :: Language
    , test_target_bin      :: FilePath
    , test_target_input    :: Maybe FilePath
    , test_target_output   :: FilePath
  } deriving (Show, Eq)

data RunTarget = RunTarget {
      run_target_problem  :: Problem
    , run_target_language :: Language
    , run_target_bin      :: FilePath
    , run_target_input    :: Maybe FilePath
  } deriving (Show, Eq)

--[ Monads ]--

type Eden c = ExceptT EdenError ( WriterT ExecutionForest (ReaderT (GlobalOptions, c) IO))

type EdenError = String

type EdenInit       = Eden InitOptions
type EdenGenerate   = Eden GenerateOptions
type EdenBuild      = Eden BuildOptions
type EdenTest       = Eden TestOptions
type EdenRun        = Eden RunOptions
type EdenPublish    = Eden PublishOptions
type EdenStatistics = Eden StatisticsOptions

type EdenMake       = Eden ()


--[ Euler Problems ]--

type Problem = Int
type Language = String
