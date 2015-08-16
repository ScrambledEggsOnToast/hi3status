module Hi3Status.Blocks.Counter
  ( CounterBlock (..) 
  ) where

import Hi3Status.Block

import qualified Data.Text as T

data CounterBlock = CounterBlock {counter :: Int}

instance Block CounterBlock where
    runBlock b = do
        pushBlockDescription $ emptyBlockDescription { full_text = T.pack . show $ counter b }
        waitForUpdateSignal 
        runBlock (b { counter = 1 + counter b })
