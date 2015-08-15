module Block.Backlight 
  ( BacklightBlock (BacklightBlock)
  ) where

import Block
import Block.Util

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.Text.Format as TF
import Data.String

data BacklightBlock = BacklightBlock { format :: String }

instance Block BacklightBlock where
    runBlock (BacklightBlock f) = onUpdate $ do
        perc <- read <$> runProcess "xbacklight" [] :: BlockM Float
        let s = TF.format (fromString f) (Only $ fixed 0 perc)
        pushBlockDescription $ emptyBlockDescription { full_text = TL.toStrict s }
