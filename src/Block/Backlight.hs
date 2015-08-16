module Block.Backlight 
  ( BacklightBlock (BacklightBlock)
  ) where

import Block
import Block.Util

import qualified Data.Text as T

import Control.Monad.IO.Class
import System.Process

data BacklightBlock = BacklightBlock { format :: String }

instance Block BacklightBlock where
    runBlock b = onUpdate $ do
        perc <- xbacklight
        let s = formatText [("perc", show $ round perc)] $ format b
        pushBlockDescription $ emptyBlockDescription { full_text = s }

xbacklight :: BlockM Float
xbacklight = read <$> (liftIO $ readProcess "xbacklight" [] "")
