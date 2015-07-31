module Main where

import           Parser
import           Types
import           Generate
import           Init


main :: IO ()
main = do
    options <- getOptions

    let g = opt_global options
        c = opt_command options

    case c of
        Init o      -> runEdenInitialiser   initE       (g, o)
        Generate o  -> runEdenGenerator     generate    (g, o)
        Build o     -> runEdenBuilder       build       (g, o)
        Test o      -> runEdenTester        test        (g, o)
        Run o       -> runEdenRunner        run         (g, o)
        Publish o   -> runEdenPublisher     publish     (g, o)

build :: EdenBuild ()
build = return ()

test :: EdenTest ()
test = return ()

run :: EdenRun ()
run = return ()

publish :: EdenPublish ()
publish = return ()
