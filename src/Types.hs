module Types
    (
      module Types
    , module Control.Monad.Except
    , module Control.Monad.IO.Class
    , module Control.Monad.Reader
    ) where

import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT, ask, asks, runReaderT)



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
  deriving Show

data BuildOptions = BuildOptions {
        build_target :: BuildTarget
    } deriving Show

data BuildTarget = BuildTarget {
        target_problem  :: Problem
    ,   target_language :: Language
    ,   target_makefile :: Maybe FilePath
    ,   target_makerule :: Maybe String
    } deriving Show

data TestOptions = TestOptions
  deriving Show

data RunOptions = RunOptions {
        run_target :: RunTarget
    } deriving Show

data RunTarget = RunTarget {
        run_target_problem  :: Problem
    ,   run_target_language :: Language
    ,   run_target_input    :: Maybe FilePath
    } deriving Show

data PublishOptions = PublishOptions
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

runEden :: Eden c a -> (GlobalOptions, c) -> IO (Either EdenError a)
runEden func opts = runReaderT (runExceptT func) opts

askEden :: (c -> a) -> Eden c a
askEden func = do
    (_, o) <- ask
    return $ func o

askGlobal :: (GlobalOptions -> a) -> Eden c a
askGlobal func = do
    (o, _) <- ask
    return $ func o


--[ Euler Problems ]--

type Problem = Int
type Language = String

type Solution = (Problem, Language)

