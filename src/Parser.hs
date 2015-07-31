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
      command "init"     parseInit
    , command "generate" (info parseGenerate idm)
    , command "build"    (info parseBuild idm)
    , command "test"     (info parseTest idm)
    , command "run"      (info parseRun idm)
    , command "publish"  (info parsePublish idm)
    ]


parseInit :: ParserInfo Command
parseInit = info parser modifier
  where
    parser = pure $ Init InitOptions
    modifier = fullDesc 
            <> progDesc "Initialise an eden repository"
            

parseGenerate :: Parser Command
parseGenerate = Generate <$> parseGenerateOptions

parseGenerateOptions :: Parser GenerateOptions
parseGenerateOptions = GenerateOptions <$> parseGenerationTarget

parseGenerationTarget :: Parser GenerationTarget
parseGenerationTarget = subparser $ mconcat
    [
      command "problem" (info parseProblemTarget idm)
    ]

parseProblemTarget :: Parser GenerationTarget
parseProblemTarget = Problem
    <$> argument auto (help "the number of the problem to generate for"
                    <> metavar "PROBLEM")

parseSolutionTarget :: Parser GenerationTarget
parseSolutionTarget = Solution
    <$> argument auto (help "the number of the problem to generate for"
                    <> metavar "PROBLEM")
    <*> argument auto (help "the language of the solution to generate for"
                    <> metavar "LANGUAGE")

parseBuild :: Parser Command
parseBuild = pure $ Build BuildOptions

parseTest :: Parser Command
parseTest = pure $ Test TestOptions

parseRun :: Parser Command
parseRun = pure $ Run RunOptions

parsePublish :: Parser Command
parsePublish = pure $ Publish PublishOptions

