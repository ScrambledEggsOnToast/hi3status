{-|
Module      : Hi3Status.Block.Util
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental

This module contains a number of common functions that are useful for writing 
blocks.

-}

{-# LANGUAGE OverloadedStrings #-}

module Hi3Status.Block.Util (
    -- * BlockM
    onUpdate,
    periodic,
    periodic_,
    -- * Text formatting
    formatText
    ) where

import Hi3Status.Block

import Control.Concurrent

import Control.Monad.IO.Class

import qualified Data.Text as T

-- | Perform a given 'BlockM' when an update is required.
onUpdate :: BlockM () -> BlockM ()
onUpdate blockm = do
    waitForUpdateSignal
    blockm
    onUpdate blockm

-- | Perform a given 'BlockM' every given number of microseconds, or whenever
-- an update is requested.
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

-- | Perform a given 'BlockM' every given number of microseconds.
periodic_ :: Int -> BlockM () -> BlockM ()
periodic_ t blockm = do
    blockm
    liftIO $ threadDelay t
    periodic_ t blockm

-- | Format a string using the given substitutions and return it as a
-- 'Data.Text.Text'.
--
-- >>> formatText [("status","Ready"),("percentage","85")] "{status}: {percentage}%" = "Ready: 85%"
formatText :: [(String, String)] -> String -> T.Text
formatText subs format = foldl (flip ($)) (T.pack format) 
                            $ map makeFormatter subs
  where 
    makeFormatter (t,s) = T.replace (T.concat ["{",T.pack t,"}"]) (T.pack s)
