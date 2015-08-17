{-|
Module      : Hi3Status.Blocks.Volume
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental
-}
module Hi3Status.Blocks.Volume 
  ( VolumeBlock (..)
  ) where

import Hi3Status.Block
import Hi3Status.Block.Util

import qualified Data.Text as T

import Control.Monad.IO.Class
import System.Process

import Text.Regex.PCRE

-- | A block to indicate the status of the system volume. Uses @amixer@ as a backend.
data VolumeBlock = VolumeBlock {
    -- | The format of the displayed text when not muted.
    --
    -- * @{icon}@ = Icon
    -- * @{perc}@ = Percentage
    format :: String,
    -- | The format of the displayed text when muted.
    muteFormat :: String,
    -- | The icon to display when not muted and volume is normal.
    normalIcon :: String,
    -- | The icon to display when not muted and volume is low.
    lowIcon :: String,
    -- | The icon to dipslay when muted.
    muteIcon :: String,
    -- | The device to query.
    device :: String,
    -- | The control to query.
    control :: String
    }

instance Block VolumeBlock where
    runBlock b = onUpdate $ do
        (perc, muted) <- amixer (device b) (control b)
        let i = if muted then muteIcon b else (if perc <= 30 then lowIcon b else normalIcon b)
            str = formatText [("icon",i),("perc",show perc)] $ if muted then muteFormat b else format b
        pushBlockDescription $ emptyBlockDescription { full_text = str }

amixer :: String -> String -> BlockM (Int, Bool)
amixer d c = do
    str <- liftIO $ readProcess "amixer" ["-D",d,"sget",c] ""
    let perc = read $ str =~ "(?<=\\[)[0-9]*(?=%\\])"
        muted = "off" == str =~ "(?<=\\[)(on|off)(?=\\])"
    return (perc, muted)
        
