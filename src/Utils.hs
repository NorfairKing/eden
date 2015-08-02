module Utils where

import           System.Process (readProcess, system)


runCommand :: String -> IO String
runCommand str = readProcess bin args ""
  where (bin:args) = words str

runSilent :: String -> IO ()
runSilent cmd = undefined cmd

runRaw :: String -> IO ()
runRaw cmd = do
    _ <- system cmd
    return ()

