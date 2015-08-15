module Block.Counter where

import Block
import qualified Data.Text as T
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

data CounterBlock = CounterBlock {counter :: Int} deriving Show

instance Block CounterBlock where
    runBlock b pos u c = do
        writeChan c . BlockUpdate pos $ emptyBlockDescription { full_text = T.pack . show $ counter b }
        _ <- takeMVar u
        runBlock (b { counter = 1 + counter b }) pos u c
