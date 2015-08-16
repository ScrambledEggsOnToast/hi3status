module Hi3Status.Blocks.Clock
  ( ClockBlock (..)
  ) where

import Hi3Status.Block
import Hi3Status.Block.Util

import qualified Data.Text as T

import Data.Time.Clock
import Data.Time.Format
import Control.Monad.IO.Class

data ClockBlock = ClockBlock { format :: String }

instance Block ClockBlock where
    runBlock (ClockBlock format) = periodic_ 1000000 $ do
        t <- liftIO getCurrentTime
        let s = formatTime defaultTimeLocale format t
        pushBlockDescription $ emptyBlockDescription { full_text = T.pack s }

