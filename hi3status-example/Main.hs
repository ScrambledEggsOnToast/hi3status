{-# LANGUAGE OverloadedStrings #-}

import Hi3Status
import Hi3Status.Blocks.Backlight
import Hi3Status.Blocks.Clock
import Hi3Status.Blocks.Counter
import Hi3Status.Blocks.Battery
import Hi3Status.Blocks.Volume
import Hi3Status.Blocks.Wifi
 
main = startStatusLine $
         "volume" %% VolumeBlock "{icon} {perc}%" "{icon}" "\61480" "\61479" "\61478" "default" "Master"
      :& "backlight" %% BacklightBlock "\61829 {perc}%"
      :& "wifi" %% WifiBlock "\61931 {ssid}" "\61931" (Just "#00ff00") (Just "#ff0000") "wlp2s0"
      :& "battery" %% BatteryBlock "{icon} {perc}% {time}" ["\62020","\62019","\62018","\62017","\62016"] " ⚡" Nothing (Just "#ff0000") (Just "#00ff00") 30
      :& "datetime" %% ClockBlock "\61463 %H:%M:%S  \61555 %d/%m/%Y"
      :& EndBlock
