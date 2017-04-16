{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Utils where

import           System.Exit           (ExitCode (..))
import           System.FilePath.Posix (hasExtension)
import           System.IO             (hGetContents)
import           System.Process        (readProcess, runInteractiveCommand,
                                        waitForProcess)

import           Eden
import           Types

fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x

snd3 :: (a, b, c) -> b
snd3 (_,x,_) = x

thd3 :: (a, b, c) -> c
thd3 (_,_,x) = x

runCommandWithInput :: String -> FilePath -> IO String
runCommandWithInput str inf = do
    instr <- readFile inf
    readProcess bin args instr
  where (bin:args) = words str

runCommand :: String -> IO String
runCommand str = readProcess bin args ""
  where (bin:args) = words str

runCommandWithTiming :: String -> IO (String, Integer)
runCommandWithTiming cmd = do
  start <- fmap read $ runCommand "date +%s%N"
  str <- runCommand cmd
  end <- fmap read $ runCommand "date +%s%N"
  return (str, end - start)

runCommandWithTimingAndInput :: String -> FilePath -> IO (String, Integer)
runCommandWithTimingAndInput cmd inf = do
  start <- fmap read $ runCommand "date +%s%N"
  str <- runCommandWithInput cmd inf
  end <- fmap read $ runCommand "date +%s%N"
  return (str, end - start)

runRaw :: String -> Eden c ()
runRaw cmd = do
    printIf (askGlobal opt_commands) cmd
    (_, outh, errh, ph) <- liftIO $ runInteractiveCommand cmd
    ec <- liftIO $ waitForProcess ph
    case ec of
        ExitSuccess -> return ()
        ExitFailure code -> do
            out   <- liftIO $ hGetContents outh
            err   <- liftIO $ hGetContents errh
            throwError $ unlines $
                [
                    unwords ["Command", show cmd, "exited with code", show code]
                ,   out
                ,   err
                ]

printIf :: Eden c Bool -> String -> Eden c ()
printIf bool str = do
    b <- bool
    if b
    then liftIO $ putStrLn str
    else return ()


notImplementedYet :: (Monad m, MonadError String m) => m ()
notImplementedYet = throwError "This feature is not implemented yet."

padN :: Int -> String -> String
padN i = padNWith i ' '

padNWith :: Int -> Char -> String -> String
padNWith m c s = replicate (m - len) c ++ s
  where len = length s

realDir :: FilePath -> Bool
realDir d | d == "."            = False
          | d == ".."           = False
          | hasExtension d      = False
          | otherwise           = True

readFromFile :: Read a => FilePath -> Eden c a
readFromFile fp = do
    str <- liftIO $ readFile fp
    return $ read str
