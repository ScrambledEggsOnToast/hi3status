{-# LANGUAGE GADTs, TypeOperators, OverloadedStrings, UnicodeSyntax #-}

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import System.IO

import Data.Time.Clock
import Data.Time.Format

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B

import qualified Data.Aeson as A
import Data.Aeson ((.=))

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import DBus
import DBus.Client

import Data.String

import Block

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
    putStr ",\n"
    receiveUpdates c ds
    
printDescriptions :: MV.IOVector BlockDescription -> IO ()
printDescriptions ds = do
    let l = MV.length ds
    mapM (\n -> do
        d <- MV.read ds n
        putStr $ show d ++ ", "
        ) [0 .. (l-1)]
    putStrLn ""

data StaticTextBlock = StaticTextBlock T.Text (Maybe T.Text) deriving Show

instance Block StaticTextBlock where
    runBlock (StaticTextBlock text color) pos u c = do
        _ <- takeMVar u
        writeChan c . BlockUpdate pos $ emptyBlockDescription { full_text = text, color = color }

staticTextBlock text color = StaticTextBlock text color

data ClockBlock = ClockBlock deriving Show

instance Block ClockBlock where
    runBlock b pos u c = do
        forkIO timer
        updateClock
      where
        updateClock = do
            t <- getCurrentTime
            let s = formatTime defaultTimeLocale "%H:%M:%S %d/%m/%Y" t
            writeChan c . BlockUpdate pos $ emptyBlockDescription { full_text = T.pack s }
            _ <- takeMVar u
            updateClock
        timer = do
            threadDelay 1000000
            putMVar u UpdateSignal
            timer

data CounterBlock = CounterBlock {counter :: Int} deriving Show

instance Block CounterBlock where
    runBlock b pos u c = do
        writeChan c . BlockUpdate pos $ emptyBlockDescription { full_text = T.pack . show $ counter b }
        _ <- takeMVar u
        runBlock (b { counter = 1 + counter b }) pos u c

update u = tryPutMVar u UpdateSignal >> return ()

updateAll us = mapM_ (\(_,u) -> update u) us

startStatusLine :: Blocks -> IO ()
startStatusLine blocks = do
    hSetBuffering stdout LineBuffering -- set buffering correctly
    putStrLn "{\"version\": 1, \"click_events\": true}\n[" -- i3bar protocol header

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

main = startStatusLine sampleBlocks
  where
    sampleBlocks = "hello" %% staticTextBlock "Hello \61441" Nothing
                :& "time" %% ClockBlock
                :& "counter" %% CounterBlock 0
                :& EndBlock
