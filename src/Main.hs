module Main where

import           Build
import           Eden
import           Execution
import           Generate
import           Init
import           Make
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

    (ee, eg) <- case c of
        Init o          -> runEden        initE         (g, o)
        Generate o gt   -> runCheckedEden (generate gt) (g, o)
        Build o t       -> runCheckedEden (build t)     (g, o)
        Test o t        -> runCheckedEden (test t)      (g, o)
        Run o t         -> runCheckedEden (run t)       (g, o)
        Publish o pt    -> runCheckedEden (publish pt)  (g, o)
        Statistics o    -> runCheckedEden statistics    (g, o)
    case ee of
        Left err  -> error err
        Right ()    -> runEdenMake (executeGraph eg) g
