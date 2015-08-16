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


--[ Monads ]--

type Eden c = ExceptT EdenError (ReaderT (GlobalOptions, c) IO)

type EdenError = String

type EdenInit       = Eden InitOptions
type EdenGenerate   = Eden GenerateOptions
type EdenBuild      = Eden BuildOptions
type EdenTest       = Eden TestOptions
type EdenRun        = Eden RunOptions
type EdenPublish    = Eden PublishOptions
type EdenStatistics = Eden StatisticsOptions


--[ Euler Problems ]--

type Problem = Int
type Language = String
