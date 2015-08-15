{-# LANGUAGE OverloadedStrings #-}

import Block
import StatusLine
import Block.Backlight
import Block.Clock
import Block.Counter
import Block.Battery
import Block.Volume
 
main = startStatusLine $
         "volume" %% VolumeBlock "{}%" "\61480" "\61479" "\61478" "default" "Master"
      :& "backlight" %% BacklightBlock "\61829 {}%"
      :& "battery" %% BatteryBlock "{}%" ["\62020","\62019","\62018","\62017","\62016"] "" Nothing (Just "#ff0000") (Just "#00ffff") 30
      :& "datetime" %% ClockBlock "\61463 %H:%M:%S  \61555 %d/%m/%Y"
      :& EndBlock
