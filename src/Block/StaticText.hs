module Block.StaticText where

import Block
import qualified Data.Text as T
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

data StaticTextBlock = StaticTextBlock T.Text (Maybe T.Text)

instance Block StaticTextBlock where
    runBlock (StaticTextBlock text color) = do
        pushBlockDescription $ emptyBlockDescription { full_text = text, color = color }
