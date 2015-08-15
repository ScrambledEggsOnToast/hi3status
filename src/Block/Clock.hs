module Block.Clock where

import Block
import qualified Data.Text as T
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Time.Clock
import Data.Time.Format
import Control.Monad.IO.Class

data ClockBlock = ClockBlock deriving Show

instance Block ClockBlock where
    runBlock b = do
        u <- getUpdater
        liftIO $ forkIO $ timer u
        updateClock
      where
        updateClock = do
            t <- liftIO getCurrentTime
            let s = formatTime defaultTimeLocale "%H:%M:%S %d/%m/%Y" t
            pushBlockDescription $ emptyBlockDescription { full_text = T.pack s }
            waitForUpdateSignal
            updateClock
        timer u = do
            threadDelay 1000000
            u
            timer u

