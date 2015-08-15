module Block.Battery 
  ( BatteryBlock (BatteryBlock)
  ) where

import Block
import Block.Util

import qualified Data.Text as T

data BatteryBlock = BatteryBlock { format :: String, batteryIcons :: [String], chargingIcon :: String, goodColor :: Maybe String, lowColor :: Maybe String, chargingColor :: Maybe String, lowThreshold :: Int }

instance Block BatteryBlock where
    runBlock b = periodic 10000000 $ do
        (charging, perc, time) <- acpi
        let i = chooseIcon (batteryIcons b) perc
            s = i ++ " " ++ show perc ++ "% " ++ time
            c = if charging then chargingColor b
                  else if perc <= lowThreshold b then lowColor b
                      else goodColor b
        pushBlockDescription $ emptyBlockDescription { full_text = T.pack s, color = T.pack <$> c }

chooseIcon :: [String] -> Int -> String
chooseIcon [] _ = ""
chooseIcon icons perc = icons !! (round $ (realToFrac $ (length icons - 1) * perc) / 100)

acpi :: BlockM (Bool, Int, String)
acpi = do
    -- sample : ["Battery","0:","Discharging,","73%,","02:59:42","remaining"]
    s <- words <$> runProcess "acpi" []
    let charging = s !! 2 /= "Discharging,"
        perc = read . init . init $ s !! 3
        hm = init . init . init $ s !! 4
        h = read [hm !! 0, hm !! 1] :: Int
        m = read [hm !! 3, hm !! 4] :: Int
        time = show h ++ "h" ++ show m ++ "m"
    return (charging, perc, time)
