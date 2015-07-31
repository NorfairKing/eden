module Types
    (
      module Types
    , module Control.Monad.Except
    , module Control.Monad.IO.Class
    , module Control.Monad.Reader
    ) where

import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
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
             | Test TestOptions | Run RunOptions
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


--[ Euler Problems ]--

type Problem = Int
type Language = String

type Solution = (Problem, Language)


