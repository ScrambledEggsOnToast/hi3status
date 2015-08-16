{-# LANGUAGE OverloadedStrings #-}

import Block
import StatusLine
import Block.Backlight
import Block.Clock
import Block.Counter
import Block.Battery
import Block.Volume
import Block.Wifi
 
main = startStatusLine $
         "volume" %% VolumeBlock "{icon} {perc}%" "{icon}" "\61480" "\61479" "\61478" "default" "Master"
      :& "backlight" %% BacklightBlock "\61829 {perc}%"
      :& "wifi" %% WifiBlock "\61931 {ssid}" "\61931" (Just "#00ff00") (Just "#ff0000") "wlp2s0"
      :& "battery" %% BatteryBlock "{icon} {perc}% {time}" ["\62020","\62019","\62018","\62017","\62016"] " ⚡" Nothing (Just "#ff0000") (Just "#00ff00") 30
      :& "datetime" %% ClockBlock "\61463 %H:%M:%S  \61555 %d/%m/%Y"
      :& EndBlock
