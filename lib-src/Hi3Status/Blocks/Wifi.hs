{-|
Module      : Hi3Status.Blocks.Wifi
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental
-}
module Hi3Status.Blocks.Wifi
  ( WifiBlock (..)
  ) where

import Hi3Status.Block
import Hi3Status.Block.Util

import qualified Data.Text as T

import Control.Monad.IO.Class
import System.Process

-- | A wifi status indicator. Uses @iwgetid@ as a backend.
data WifiBlock = WifiBlock { 
    -- | The format of the displayed text when connected to a network.
    --
    -- * @{ssid}@ = The SSID of the network.
    connectedFormat :: String, 
    -- | The format of the displayed text when not connected to a network.
    disconnectedFormat :: String, 
    -- | The color of the text when connected to a network.
    connectedColor :: Maybe String, 
    -- | The color of the text when not connected to a network.
    disconnectedColor :: Maybe String, 
    -- | The device to query, e.g. "wlan0".
    device :: String 
    }

instance Block WifiBlock where
    runBlock b = periodic 5000000 $ do
        ms <- iwconfig (device b)
        let text = case ms of
                Nothing -> formatText [] $ disconnectedFormat b
                Just s -> formatText [("ssid",s)] $ connectedFormat b
            c = case ms of
                Nothing -> disconnectedColor b
                Just _ -> connectedColor b
        pushBlockDescription $ emptyBlockDescription { full_text = text, color = T.pack <$> c}

iwconfig :: String -> BlockM (Maybe String)
iwconfig dev = do
    ssid <- liftIO $ filter (/='\n') <$> readProcess "iwgetid" ["-r",dev] ""
    return $ if ssid == "" then Nothing else Just ssid
