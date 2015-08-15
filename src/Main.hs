{-# LANGUAGE OverloadedStrings #-}

import Block
import StatusLine
import Block.StaticText
import Block.Clock
import Block.Counter

main = do
    startStatusLine sampleBlocks
  where
    sampleBlocks = "hello" %% staticTextBlock "Hello \61441" Nothing
                :& "time" %% ClockBlock
                :& "counter" %% CounterBlock 0
                :& EndBlock
