module Parser where

import           Types

import           Options.Applicative

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (helper <*> parseOptions) help
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
parseCommand = hsubparser $ mconcat
    [
      command "init"     parseInit
    , command "generate" parseGenerate
    , command "build"    parseBuild
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


parseGenerate :: ParserInfo Command
parseGenerate = info parser modifier
  where
    parser = Generate <$> parseGenerateOptions
    modifier = fullDesc
            <> progDesc "Generate the environment."

parseGenerateOptions :: Parser GenerateOptions
parseGenerateOptions = GenerateOptions <$> parseGenerationTarget

parseGenerationTarget :: Parser GenerationTarget
parseGenerationTarget = hsubparser $ mconcat
    [
      command "problem"     (info parseProblemTarget idm)
    , command "solution"    (info parseSolutionTarget idm)
    , command "library"     (info parseLibraryTarget idm)
    , command "tests"       (info parseTestsTarget idm)
    , command "build"       (info parseBuildDirTarget idm)
    , command "environment" (info parseEnvironmentTarget idm)
    ]

parseProblemTarget :: Parser GenerationTarget
parseProblemTarget = Problem
    <$> argument auto (help "the number of the problem to generate for"
                    <> metavar "PROBLEM")

parseSolutionTarget :: Parser GenerationTarget
parseSolutionTarget = Solution
    <$> argument auto (help "the number of the problem to generate for"
                    <> metavar "PROBLEM")
    <*> argument str (help "the language of the solution to generate for"
                    <> metavar "LANGUAGE")

parseLibraryTarget :: Parser GenerationTarget
parseLibraryTarget = Library
    <$> argument str (help "the language to generate the library for"
                    <> metavar "LANGUAGE")

parseTestsTarget :: Parser GenerationTarget
parseTestsTarget = Tests
    <$> argument str (help "the language to generate the tests for"
                    <> metavar "LANGUAGE")

parseBuildDirTarget :: Parser GenerationTarget
parseBuildDirTarget = BuildDir
    <$> argument str (help "the language to generate the build dir for"
                    <> metavar "LANGUAGE")

parseEnvironmentTarget :: Parser GenerationTarget
parseEnvironmentTarget = Environment
    <$> argument str (help "the language to generate a development environment for"
                    <> metavar "LANGUAGE")



parseBuild :: ParserInfo Command
parseBuild = info (helper <*> parser) modifier
  where
    parser = Build <$> parseBuildOptions
    modifier = fullDesc
            <> progDesc "Build a solution."

parseBuildOptions :: Parser BuildOptions
parseBuildOptions = BuildOptions <$> parseBuildTarget

parseBuildTarget :: Parser BuildTarget
parseBuildTarget = BuildTarget
    <$> argument    auto
        (help "the number of the problem for which to build the solution"
        <> metavar "PROBLEM")
    <*> argument    str
        (help "the language of the problem for which to build the solution"
        <> metavar "LANGUAGE")
    <*> option      (Just <$> str)
        (help "the makefile to use for this build (relative to eden root or absolute)"
        <> long "makefile"
        <> short 'm'
        <> value Nothing
        <> metavar "MAKEFILE")
    <*> option      (Just <$> str)
        (help "the make fule to use for this build"
        <> long "rule"
        <> short 'r'
        <> value Nothing
        <> metavar "MAKERULE")

parseTest :: Parser Command
parseTest = pure $ Test TestOptions

parseRun :: Parser Command
parseRun = pure $ Run RunOptions

parsePublish :: Parser Command
parsePublish = pure $ Publish PublishOptions

