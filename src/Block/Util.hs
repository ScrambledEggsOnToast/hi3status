{-# LANGUAGE OverloadedStrings #-}

module Block.Util where

import Block

import System.Process
import System.Exit
import GHC.IO.Handle

import Control.Concurrent

import Control.Monad.IO.Class

import qualified Data.Text as T

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

formatText :: [(String, String)] -> String -> T.Text
formatText subs format = foldl (flip ($)) (T.pack format) $ map makeFormatter subs
  where makeFormatter (t,s) = T.replace (T.concat ["{",T.pack t,"}"]) (T.pack s)
