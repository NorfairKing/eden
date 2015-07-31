module Main where

import           Generate
import           Init
import           Parser
import           Paths
import           Types


main :: IO ()
main = do
    options <- getOptions

    let g = opt_global options
        c = opt_command options

    ee <- case c of
        Init o      -> runEden initE       (g, o)
        Generate o  -> runEden generate    (g, o)
        Build o     -> runEden build       (g, o)
        Test o      -> runEden test        (g, o)
        Run o       -> runEden run         (g, o)
        Publish o   -> runEden publish     (g, o)
    case ee of
        Left error  -> putStrLn error
        Right ()    -> return ()

build :: EdenBuild ()
build = do
    mroot <- edenRoot
    liftIO $ print mroot

test :: EdenTest ()
test = return ()

run :: EdenRun ()
run = return ()

publish :: EdenPublish ()
publish = return ()
