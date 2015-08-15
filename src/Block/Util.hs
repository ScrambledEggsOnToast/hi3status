module Block.Util where

import Block

import System.Process
import System.Exit
import GHC.IO.Handle

import Control.Concurrent

import Control.Monad.IO.Class

-- run a process and return its stdout
runProcess :: FilePath -> [String] -> BlockM String
runProcess fp args = liftIO $ do
    (_, mo, _, p) <- createProcess $ (proc fp args) { std_out = CreatePipe }
    case mo of
        Nothing -> error $ "subprocess error: " ++ fp
        Just o -> hGetContents o

-- perform a given blockm at an update signal
onUpdate :: BlockM () -> BlockM ()
onUpdate blockm = do
    waitForUpdateSignal
    blockm
    onUpdate blockm

-- perform a given blockm every given number of microseconds
periodic :: Int -> BlockM () -> BlockM ()
periodic t blockm = do
    u <- getUpdater
    liftIO $ forkIO $ timer u
    go
  where
    go = do
        blockm
        waitForUpdateSignal
        go
    timer u = do
        threadDelay t
        u
        timer u

-- version of periodic that doesn't accept external update requests
periodic_ :: Int -> BlockM () -> BlockM ()
periodic_ t blockm = do
    blockm
    liftIO $ threadDelay t
    periodic_ t blockm
