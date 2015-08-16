module Block.Volume 
  ( VolumeBlock (VolumeBlock)
  ) where

import Block
import Block.Util

import qualified Data.Text as T

import Control.Monad.IO.Class
import System.Process

import Text.Regex.PCRE

data VolumeBlock = VolumeBlock { format :: String, muteFormat :: String, normalIcon :: String, lowIcon :: String, muteIcon :: String, device :: String, control :: String }

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
        
