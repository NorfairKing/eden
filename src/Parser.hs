module Parser where

import           Types

import           Options.Applicative

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info parseOptions help
    help = fullDesc <> progDesc description
    description = "Euler Development ENgine"

parseOptions :: Parser Options
parseOptions = Options
    <$> parseGlobalOptions
    <*> parseCommand

parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions = GlobalOptions
    <$> switch (long "debug" <> help "give debug output")


parseCommand :: Parser Command
parseCommand = subparser $ mconcat
    [
      command "generate" (info parseGenerateOptions idm)
    , command "build"    (info parseBuildOptions idm)
    , command "test"     (info parseTestOptions idm)
    , command "run"      (info parseRunOptions idm)
    , command "publish"  (info parsePublishOptions idm)
    ]

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

