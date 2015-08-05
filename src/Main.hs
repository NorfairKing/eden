module Main where

import           Build
import           Eden
import           Generate
import           Init
import           Parser
import           Publish
import           Run
import           Test
import           Types


main :: IO ()
main = do
    options <- getOptions

    let g = opt_global options
        c = opt_command options

    ee <- case c of
        Init o      -> runEden        initE       (g, o)
        Generate o  -> runCheckedEden generate    (g, o)
        Build o     -> runCheckedEden build       (g, o)
        Test o      -> runCheckedEden test        (g, o)
        Run o       -> runCheckedEden run         (g, o)
        Publish o   -> runCheckedEden publish     (g, o)
    case ee of
        Left error  -> putStrLn error
        Right ()    -> return ()
