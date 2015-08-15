module Block.Counter where

import Block
import qualified Data.Text as T
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

data CounterBlock = CounterBlock {counter :: Int}

instance Block CounterBlock where
    runBlock b = do
        pushBlockDescription $ emptyBlockDescription { full_text = T.pack . show $ counter b }
        waitForUpdateSignal 
        runBlock (b { counter = 1 + counter b })
