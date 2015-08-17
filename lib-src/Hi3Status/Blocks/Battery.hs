{-|
Module      : Hi3Status.Blocks.Battery
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental
-}
module Hi3Status.Blocks.Battery 
  ( BatteryBlock (..)
  ) where

import Hi3Status.Block
import Hi3Status.Block.Util

import qualified Data.Text as T

import Control.Monad.IO.Class
import System.Process

import Text.Regex.PCRE
import Text.Read
import Data.Maybe

-- | A battery indicator. Uses @acpi@ as a backend.
data BatteryBlock = BatteryBlock {
    -- | The format of the display text.
    --
    -- * @{icon}@ = Icon
    -- * @{perc}@ = Percentage charge
    -- * @{time}@ = Time remaining
    format :: String,
    -- | A list of icons for the battery in order of increasing charge.
    batteryIcons :: [String],
    -- | An icon to display when the battery is charging.
    chargingIcon :: String,
    -- | The color to use when the battery has plenty of charge.
    goodColor :: Maybe String,
    -- | The color to use when the battery is low on charge.
    lowColor :: Maybe String,
    -- | The color to use when the battery is charging.
    chargingColor :: Maybe String,
    -- | For percentages below this threshold, 'lowColor' is used; for 
    -- percentages above, 'goodColor' is used.
    lowThreshold :: Int 
    }

instance Block BatteryBlock where
    runBlock b = periodic 5000000 $ do
        (charging, perc, time) <- acpi
        let i = chooseIcon (batteryIcons b) perc ++ if charging then chargingIcon b else ""
            text = formatText [("icon", i),("perc", show perc),("time",time)] $ format b
            c = if charging then chargingColor b
                  else if perc <= lowThreshold b then lowColor b
                      else goodColor b
        pushBlockDescription $ emptyBlockDescription { full_text = text, color = T.pack <$> c }

chooseIcon :: [String] -> Int -> String
chooseIcon [] _ = ""
chooseIcon icons perc = icons !! (round $ (realToFrac $ (length icons - 1) * perc) / 100)

acpi :: BlockM (Bool, Int, String)
acpi = do
    s <- liftIO $ readProcess "acpi" [] ""
    let charging = s =~ "Charging" :: Bool
        perc = read $ s =~ "[0-9]*(?=%)" :: Int
        mh = readMaybe $ s =~ "[0-9][0-9](?=:)" :: Maybe Int
        mm = readMaybe $ s =~ "(?<=:)[0-9][0-9](?=:)" :: Maybe Int
        time = fromMaybe "full" $ do
            h <- mh
            m <- mm
            return $ show h ++ "h" ++ show m ++ "m"
    return (charging, perc, time)
