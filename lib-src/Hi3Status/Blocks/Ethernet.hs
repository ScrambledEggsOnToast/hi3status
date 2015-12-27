{-|
Module      : Hi3Status.Blocks.Ethernet
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental
-}
module Hi3Status.Blocks.Ethernet
  ( EthernetBlock (..)
  ) where

import Hi3Status.Block
import Hi3Status.Block.Util

import qualified Data.Text as T

import Control.Monad.IO.Class
import System.Process

-- | An ethernet status indicator. Uses files at @/sys/class/net/@.
data EthernetBlock = EthernetBlock { 
    -- | The format of the displayed text when connected to a network.
    connectedFormat :: String, 
    -- | The format of the displayed text when not connected to a network.
    disconnectedFormat :: String, 
    -- | The color of the text when connected to a network.
    connectedColor :: Maybe String, 
    -- | The color of the text when not connected to a network.
    disconnectedColor :: Maybe String, 
    -- | The device to query, e.g. "eth0".
    device :: String 
    }

instance Block EthernetBlock where
    runBlock b = periodic 5000000 $ do
        os <- operstate (device b)
        let (c,text) = 
                if os 
                    then (connectedColor b, formatText [] (connectedFormat b))
                    else (disconnectedColor b, formatText [] (disconnectedFormat b))
        pushBlockDescription $ emptyBlockDescription { full_text = text, color = T.pack <$> c}

operstate :: String -> BlockM Bool
operstate dev = do
    os <- liftIO $ filter (/='\n') <$> readProcess "cat" ["/sys/class/net/"++dev++"/operstate"] ""
    return (os == "up")
