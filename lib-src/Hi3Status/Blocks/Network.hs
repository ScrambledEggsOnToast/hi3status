{-|
Module      : Hi3Status.Blocks.Network
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental
-}
module Hi3Status.Blocks.Network
  ( NetworkBlock (..)
  ) where

import Hi3Status.Block
import Hi3Status.Block.Util

import qualified Data.Text as T

import Data.Prefix.Units

import Control.Monad.IO.Class
import Control.Concurrent
import System.Process

-- | A network transfer rate indicator. Uses files at @/sys/class/net/@.
data NetworkBlock = NetworkBlock { 
    -- | The format of the displayed text.
    --
    -- * @{rx}@ = Download rate.
    -- * @{tx}@ = Upload rate.
    format :: String, 
    -- | The device to query, e.g. "eth0".
    device :: String,
    -- | How often to check the rates (in microseconds).
    checkPeriod :: Int
    }

instance Block NetworkBlock where
  {-  runBlock b = do
        (rx,tx) <- rxtx (device b)
        rxRef <- liftIO $ newIORef rx
        txRef <- liftIO $ newIORef tx
        periodic (checkPeriod b) $ do
            (rx',tx') <- rxtx (device b)
            liftIO $ writeIORef rx 
-}
    runBlock b = do
        (rx,tx) <- rxtx (device b)
        go rx tx
      where
        go rx tx = do
            (rx',tx') <- rxtx (device b)
            let rr = 1000000 * (realToFrac $ rx'-rx) / (realToFrac $ checkPeriod b) :: Double
                tr = 1000000 * (realToFrac $ tx'-tx) / (realToFrac $ checkPeriod b) :: Double
                rs = showValue FormatSiKMGT (round rr :: Int)
                ts = showValue FormatSiKMGT (round tr :: Int)

            pushBlockDescription $ emptyBlockDescription { full_text = formatText [("rx",rs),("tx",ts)] (format b) }
            
            liftIO $ threadDelay (checkPeriod b)
            go rx' tx'

rxtx :: String -> BlockM (Int, Int)
rxtx dev = do
    rx <- liftIO $ filter (/='\n') <$> readProcess "cat" ["/sys/class/net/"++dev++"/statistics/rx_bytes"] ""
    tx <- liftIO $ filter (/='\n') <$> readProcess "cat" ["/sys/class/net/"++dev++"/statistics/tx_bytes"] ""
    return (read rx, read tx)
