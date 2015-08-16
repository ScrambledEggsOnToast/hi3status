{-# LANGUAGE OverloadedStrings #-}

module Block.Wifi where

import Block
import Block.StaticText
import Block.Util

import qualified Data.Text as T

import Data.List

import Control.Monad.IO.Class
import System.Process


-- TODO : make this not just static text

data WifiBlock = WifiBlock { connectedFormat :: String, disconnectedFormat :: String, connectedColor :: Maybe String, disconnectedColor :: Maybe String, device :: String }

instance Block WifiBlock where
    runBlock b = onUpdate $ do
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
