module Main where

import           Build
import           Eden
import           Generate
import           Init
import qualified Parser     (getOptions)
import           Publish
import           Run
import           Statistics
import           Test
import           Types
import           Utils


main :: IO ()
main = do
    options <- Parser.getOptions

    let g = opt_global options
        c = opt_command options

    (ee, mts) <- case c of
        Init o          -> runEden        initE         (g, o)
        Generate o gt   -> runCheckedEden (generate gt) (g, o)
        Build o t       -> runCheckedEden (build t)     (g, o)
        Test o t        -> runCheckedEden (test t)      (g, o)
        Run o t         -> runCheckedEden (run t)       (g, o)
        Publish o pt    -> runCheckedEden (publish pt)  (g, o)
        Statistics o    -> runCheckedEden statistics    (g, o)
    case ee of
        Left error  -> putStrLn error
        Right ()    -> makeTargets mts
