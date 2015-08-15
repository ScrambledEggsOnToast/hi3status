module Block.StaticText where

import Block
import qualified Data.Text as T
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

data StaticTextBlock = StaticTextBlock T.Text (Maybe T.Text) deriving Show

instance Block StaticTextBlock where
    runBlock (StaticTextBlock text color) pos u c = do
        _ <- takeMVar u
        writeChan c . BlockUpdate pos $ emptyBlockDescription { full_text = text, color = color }

staticTextBlock text color = StaticTextBlock text color
