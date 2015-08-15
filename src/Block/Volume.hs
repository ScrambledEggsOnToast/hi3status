module Block.Volume 
  ( VolumeBlock (VolumeBlock)
  ) where

import Block
import Block.Util

import qualified Data.Text as T

data VolumeBlock = VolumeBlock { format :: String, normalIcon :: String, lowIcon :: String, muteIcon :: String, device :: String, control :: String }

instance Block VolumeBlock where
    runBlock b = onUpdate $ do
        (perc, muted) <- amixer (device b) (control b)
        let str = if muted then muteIcon b
                else (if perc == 0 then muteIcon b else (if  perc <= 30 then lowIcon b else normalIcon b)) ++ " " ++ show perc ++ "%"
        pushBlockDescription $ emptyBlockDescription { full_text = T.pack str }

amixer :: String -> String -> BlockM (Int, Bool)
amixer d c = do
    str <- runProcess "amixer" ["-D",d,"sget",c]
    -- sample : ["Mono:","Playback","71","[56%]","[-28.00dB]","[off]"]
    -- TODO : replace with regexp
    let s = words $ (lines str) !! 4
        perc = read . tail . init . init $ s !! 3
        muted = "[off]" == (s !! 5)
    return (perc, muted)
        
