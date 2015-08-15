module Block.Clock where

import Block
import qualified Data.Text as T
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Time.Clock
import Data.Time.Format

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

