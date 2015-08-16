module Parser where

import           Options.Applicative

import           Constants
import           Eden
import           Solutions
import           Types


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
    <$> switch
        (long "commands"
        <> help "print every executed command")


parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
    [
      command "init"     parseInit
    , command "generate" parseGenerate
    , command "build"    parseBuild
    , command "test"     parseTest
    , command "run"      parseRun
    , command "publish"  parsePublish
    , command "stats"    parseStatistics
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
    parser = Generate <$> parseGenerateOptions <*> parseGenerationTarget
    modifier = fullDesc
            <> progDesc "Generate the environment."

parseGenerateOptions :: Parser GenerateOptions
parseGenerateOptions = pure GenerateOptions

parseGenerationTarget :: Parser GenerationTarget
parseGenerationTarget = hsubparser $ mconcat
    [
      command "problem"         (info parseGenerateProblemTarget idm)
    , command "solution"        (info parseGenerateSolutionTarget idm)
    , command "library"         (info parseGenerateLibraryTarget idm)
    , command "tests"           (info parseGenerateTestsTarget idm)
    , command "build"           (info parseGenerateBuildDirTarget idm)
    , command "environment"     (info parseGenerateEnvironmentTarget idm)
    , command "writeups"        (info parseGeneratePublishingTarget idm)
    , command "getting-started" (info parseGenerateGettingStartedTarget idm)
    ]

parseGenerateProblemTarget :: Parser GenerationTarget
parseGenerateProblemTarget = Problem
    <$> argument auto (help "the number of the problem to generate for"
                    <> metavar "PROBLEM")

parseGenerateSolutionTarget :: Parser GenerationTarget
parseGenerateSolutionTarget = Solution
    <$> argument auto (help "the number of the problem to generate for"
                    <> metavar "PROBLEM")
    <*> argument str (help "the language of the solution to generate for"
                    <> metavar "LANGUAGE")

parseGenerateLibraryTarget :: Parser GenerationTarget
parseGenerateLibraryTarget = Library
    <$> argument str (help "the language to generate the library for"
                    <> metavar "LANGUAGE")

parseGenerateTestsTarget :: Parser GenerationTarget
parseGenerateTestsTarget = Tests
    <$> argument str (help "the language to generate the tests for"
                    <> metavar "LANGUAGE")

parseGenerateBuildDirTarget :: Parser GenerationTarget
parseGenerateBuildDirTarget = BuildDir
    <$> argument str (help "the language to generate the build dir for"
                    <> metavar "LANGUAGE")

parseGenerateEnvironmentTarget :: Parser GenerationTarget
parseGenerateEnvironmentTarget = Environment
    <$> argument str (help "the language to generate a development environment for"
                    <> metavar "LANGUAGE")

parseGeneratePublishingTarget :: Parser GenerationTarget
parseGeneratePublishingTarget = pure Publishing

parseGenerateGettingStartedTarget :: Parser GenerationTarget
parseGenerateGettingStartedTarget = pure GettingStarted


parseBuild :: ParserInfo Command
parseBuild = info parser modifier
  where
    parser = Build <$> parseBuildOptions <*> parseTarget
    modifier = fullDesc
            <> progDesc "Build a solution."

parseBuildOptions :: Parser BuildOptions
parseBuildOptions = BuildOptions
    <$> option      (Just <$> str)
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

parseTest :: ParserInfo Command
parseTest = info (Test <$> parseTestOptions <*> parseTarget) idm

parseTestOptions :: Parser TestOptions
parseTestOptions = pure TestOptions

parseRun :: ParserInfo Command
parseRun = info parser modifier
  where
    parser = Run <$> parseRunOptions <*> parseTarget
    modifier = fullDesc
            <> progDesc "Run a solution."

parseRunOptions :: Parser RunOptions
parseRunOptions = RunOptions
    <$> option      (Just <$> str)
        (help "the input file to be used in the solution"
        <> long "input"
        <> short 'i'
        <> value Nothing
        <> metavar "INPUT_FILE")
    <*> option      str
        (help "the binary to run"
        <> long "binary"
        <> short 'b'
        <> value defaultExecutable
        <> metavar "BINARY")

parseTarget :: Parser Target
parseTarget = hsubparser $ mconcat
        [
            command "all"       $ parseAllTarget
        ,   command "libraries" $ parseAllLibrariesTarget
        ,   command "library"   $ parseLibraryTarget
        ,   command "problems"  $ parseAllProblemsTarget
        ,   command "problem"   $ parseProblemTarget
        ,   command "solution"  $ parseSolutionTarget
        ]

parseAllTarget :: ParserInfo Target
parseAllTarget = info (pure TargetAll) idm

parseAllLibrariesTarget :: ParserInfo Target
parseAllLibrariesTarget = info (pure TargetAllLibraries) idm

parseLibraryTarget :: ParserInfo Target
parseLibraryTarget = info parser modifier
  where
    parser = TargetLibrary
             <$> argument str (help "the library to test"
                             <> metavar "LANGUAGE")
    modifier = idm

parseAllProblemsTarget :: ParserInfo Target
parseAllProblemsTarget = info (pure TargetAllProblems) idm

parseProblemTarget :: ParserInfo Target
parseProblemTarget = info parser modifier
  where
    parser = TargetProblem
             <$> argument auto (help "the problem to test"
                    <> metavar "PROBLEM")
    modifier = idm

parseSolutionTarget :: ParserInfo Target
parseSolutionTarget = info parser modifier
  where
    parser = TargetSolution
             <$> argument auto (help "the problem to test"
                    <> metavar "PROBLEM")
             <*> argument str (help "the language of the solution to test"
                    <> metavar "LANGUAGE")
    modifier = idm

parsePublish :: ParserInfo Command
parsePublish = info parser modifier
  where
    parser = Publish <$> parsePublishOptions <*> parsePublishTarget
    modifier = fullDesc
            <> progDesc "Publish writeups."

parsePublishOptions :: Parser PublishOptions
parsePublishOptions = pure PublishOptions

parsePublishTarget :: Parser PublishTarget
parsePublishTarget = hsubparser $ mconcat
    [
        command "all"       (info parsePublishAllTarget idm)
    ,   command "problem"   (info parsePublishProblemTarget idm)
    ]

parsePublishAllTarget :: Parser PublishTarget
parsePublishAllTarget = pure PublishAll

parsePublishProblemTarget :: Parser PublishTarget
parsePublishProblemTarget = PublishProblem
    <$> argument auto
        (help "the number of the problem for which to publish the explanation"
        <> metavar "PROBLEM")

parseStatistics :: ParserInfo Command
parseStatistics = info parser modifier
  where
    parser = Statistics <$> parseStatisticsOptions
    modifier = fullDesc
            <> progDesc "Get statistics about an eden repository"

parseStatisticsOptions :: Parser StatisticsOptions
parseStatisticsOptions = pure StatisticsOptions
