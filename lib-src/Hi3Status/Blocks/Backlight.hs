{-|
Module      : Hi3Status.Blocks.Backlight
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental
-}

module Hi3Status.Blocks.Backlight 
  ( BacklightBlock (..)
  ) where

import Hi3Status.Block
import Hi3Status.Block.Util

import qualified Data.Text as T

import Control.Monad.IO.Class
import System.Process

-- | A backlight percentage brightness indicator. Uses @xbacklight@ as a backend.
data BacklightBlock = BacklightBlock { 
    -- | The format of the display text. 
    --
    -- * @{perc}@ = backlight brightness percentage.
    format :: String 
    }

instance Block BacklightBlock where
    runBlock b = onUpdate $ do
        perc <- xbacklight
        let s = formatText [("perc", show $ round perc)] $ format b
        pushBlockDescription $ emptyBlockDescription { full_text = s }

xbacklight :: BlockM Float
xbacklight = read <$> (liftIO $ readProcess "xbacklight" [] "")
