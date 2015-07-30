module Main where

import Options.Applicative

data Options = Options
    { opt_quiet :: Bool,
      opt_command :: Command }
  deriving Show

data BuildOptions = BuildOptions
  deriving Show
data RunOptions = RunOptions
  deriving Show

data Command = Build BuildOptions
             | Run RunOptions
  deriving Show

buildOptions :: Parser Command
buildOptions = pure $ Build BuildOptions

runOptions :: Parser Command
runOptions = pure $ Run RunOptions


build :: IO ()
build = putStrLn "build"

run :: IO ()
run = putStrLn "run"

commandParser :: Parser Command
commandParser = subparser
    ( command "build" (info buildOptions idm)
   <> command "run"   (info runOptions idm))

options :: Parser Options
options = Options
     <$> switch
         ( long "quiet"
        <> help "Whether to be quiet" )
     <*> commandParser

eden :: Options -> IO ()
eden = print


main :: IO ()
main = execParser opts >>= eden
  where
    opts = info options
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
