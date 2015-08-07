module Parser where

import           Constants
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
    , command "writeups"    (info parsePublishingTarget idm)
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

parsePublishingTarget :: Parser GenerationTarget
parsePublishingTarget = pure Publishing


parseBuild :: ParserInfo Command
parseBuild = info parser modifier
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

parseTest :: ParserInfo Command
parseTest = info parser modifier
  where
    parser = Test <$> parseTestOptions
    modifier = fullDesc
            <> progDesc "Test a test target"

parseTestOptions :: Parser TestOptions
parseTestOptions = TestOptions <$> parseTestTarget

parseTestTarget :: Parser TestTarget
parseTestTarget = hsubparser $ mconcat
    [
        command "all"       $ info parseAllTestTarget idm
    ,   command "libraries" $ info parseAllLibrariesTestTarget idm
    ,   command "library"   $ info parseLibraryTestTarget idm
    ,   command "problems"  $ info parseAllProblemsTestTarget idm
    ,   command "problem"   $ info parseProblemTestTarget idm
    ,   command "solution"  $ info parseSolutionTestTarget idm
    ]

parseAllTestTarget :: Parser TestTarget
parseAllTestTarget = pure TestTargetAll

parseAllLibrariesTestTarget :: Parser TestTarget
parseAllLibrariesTestTarget = pure TestTargetAllLibraries

parseLibraryTestTarget :: Parser TestTarget
parseLibraryTestTarget = TestTargetLibrary
    <$> argument str (help "the library to test"
                    <> metavar "LANGUAGE")

parseAllProblemsTestTarget :: Parser TestTarget
parseAllProblemsTestTarget = pure TestTargetAllProblems

parseProblemTestTarget :: Parser TestTarget
parseProblemTestTarget = TestTargetProblem
    <$> argument auto (help "the problem to test"
                    <> metavar "PROBLEM")

parseSolutionTestTarget :: Parser TestTarget
parseSolutionTestTarget = TestTargetSolution
    <$> argument auto (help "the problem to test"
                    <> metavar "PROBLEM")
    <*> argument str (help "the language of the solution to test"
                    <> metavar "LANGUAGE")

parseRun :: ParserInfo Command
parseRun = info parser modifier
  where
    parser = Run <$> parseRunOptions
    modifier = fullDesc
            <> progDesc "Run a solution."

parseRunOptions :: Parser RunOptions
parseRunOptions = RunOptions <$> parseRunTarget

parseRunTarget :: Parser RunTarget
parseRunTarget = RunTarget
    <$> argument    auto
        (help "the number of the problem for which to run the solution"
        <> metavar "PROBLEM")
    <*> argument    str
        (help "the language of the problem for which to run the solution"
        <> metavar "LANGUAGE")
    <*> option      (Just <$> str)
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

parsePublish :: ParserInfo Command
parsePublish = info parser modifier
  where
    parser = Publish <$> parsePublishOptions
    modifier = fullDesc
            <> progDesc "Publish writeups."

parsePublishOptions :: Parser PublishOptions
parsePublishOptions = PublishOptions <$> parsePublishTarget

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
