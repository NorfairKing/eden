module Types where

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
