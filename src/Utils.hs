module Utils where

import           System.Process (readProcess, system)
import           Types


runCommand :: String -> IO String
runCommand str = readProcess bin args ""
  where (bin:args) = words str

runSilent :: String -> IO ()
runSilent cmd = undefined cmd

runRaw :: String -> Eden c ()
runRaw cmd = do
    printIf (askGlobal opt_commands) cmd

    _ <- liftIO $ system cmd
    return ()

printIf :: Eden c Bool -> String -> Eden c ()
printIf bool str = do
    b <- bool
    if b
    then liftIO $ putStrLn str
    else return ()
