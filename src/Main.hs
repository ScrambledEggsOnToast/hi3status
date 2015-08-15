{-# LANGUAGE OverloadedStrings #-}

import Block
import StatusLine
import Block.Backlight
import Block.Clock
import Block.Counter

main = startStatusLine $
         "backlight" %% BacklightBlock "\61829 {}%"
      :& "datetime" %% ClockBlock "\61463 %H:%M:%S  \61555 %d/%m/%Y"
      :& EndBlock
