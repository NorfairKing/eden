module Main where

import           Options.Applicative

data Options = Options
    { opt_quiet   :: Bool,
      opt_command :: Command }
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

data Command = Generate GenerateOptions
             | Build BuildOptions
             | Test TestOptions
             | Run RunOptions
             | Publish PublishOptions
  deriving Show

parseGenerateOptions :: Parser Command
parseGenerateOptions = pure $ Generate GenerateOptions

parseBuildOptions :: Parser Command
parseBuildOptions = pure $ Build BuildOptions

parseTestOptions :: Parser Command
parseTestOptions = pure $ Test TestOptions

parseRunOptions :: Parser Command
parseRunOptions = pure $ Run RunOptions

parsePublishOptions :: Parser Command
parsePublishOptions = pure $ Publish PublishOptions

commandParser :: Parser Command
commandParser = subparser $ mconcat
    [
      command "generate" (info parseGenerateOptions idm)
    , command "build"    (info parseBuildOptions idm)
    , command "test"     (info parseTestOptions idm)
    , command "run"      (info parseRunOptions idm)
    , command "publish"  (info parsePublishOptions idm)
    ]

options :: Parser Options
options = Options
     <$> switch (long "debug" <> help "give debug output")
     <*> commandParser

eden :: Options -> IO ()
eden = print


main :: IO ()
main = execParser opts >>= eden
  where
    opts = info options help
    help = fullDesc <> progDesc description

description :: String
description = "Euler Development ENgine"
