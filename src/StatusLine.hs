{-# LANGUAGE OverloadedStrings #-}
module StatusLine where

import Block

import System.IO

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import qualified Data.ByteString.Lazy.Char8 as B
import Data.String

import DBus
import DBus.Client

import qualified Data.Aeson as A

runBlocks :: Blocks -> Chan BlockUpdate -> IO [(String, MVar UpdateSignal)]
runBlocks bs c = runBlocks' 0 bs c
runBlocks' _ EndBlock _ = return []
runBlocks' n (BlocksEntry i b :& bs) c = do
    u <- newMVar UpdateSignal
    forkIO (runBlock b n u c)
    ius <- runBlocks' (n+1) bs c
    return ((i,u):ius)

receiveUpdates :: Chan BlockUpdate -> MV.IOVector BlockDescription -> IO ()
receiveUpdates c ds = do
    BlockUpdate n d <- readChan c
    MV.write ds n d
    fds <- V.freeze ds
    let jds = A.toJSON fds
        out = A.encode jds
    B.putStr out
    putStr ","
    receiveUpdates c ds

update u = tryPutMVar u UpdateSignal >> return ()

updateAll us = mapM_ (\(_,u) -> update u) us

startStatusLine :: Blocks -> IO ()
startStatusLine blocks = do
    hSetBuffering stdout LineBuffering -- set buffering correctly

    putStr "{\"version\": 1, \"click_events\": true}[" -- i3bar protocol header

    -- The channel for block updates
    updateChan <- newChan :: IO (Chan BlockUpdate)

    -- Initiate the blockdescriptions with empty blocks
    blockDescriptions <- MV.replicate (blockCount blocks) emptyBlockDescription :: IO (MV.IOVector BlockDescription)

    -- Start the blocks, and obtain their names/updaters
    namesUpdaters <- runBlocks blocks updateChan

    -- Connect to d-bus
    client <- connectSession 
    requestName client "org.i3wm.hi3status" [nameAllowReplacement, nameReplaceExisting]

    -- Set up d-bus methods
    export client "/" [autoMethod "org.i3wm.hi3status" "Update" $ updateAll namesUpdaters]
    mapM_ (\(name,updater) -> do
        export client (fromString $ "/"++name) [autoMethod "org.i3wm.hi3status" "Update" $ update updater]
        return ()) namesUpdaters
        

    -- Start receiving and handling updates - do this last    
    receiveUpdates updateChan blockDescriptions
