{-|
Module      : Hi3Status.Blocks.StaticText
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental
-}
module Hi3Status.Blocks.StaticText (
    StaticTextBlock (..)
    ) where

import Hi3Status.Block

import qualified Data.Text as T

-- | A simple block that displays a piece of static text.
data StaticTextBlock = StaticTextBlock {
    -- | The text to display.
    text :: String, 
    -- | The color of the text.
    textColor :: (Maybe String) 
    }

instance Block StaticTextBlock where
    runBlock b = do
        pushBlockDescription $ emptyBlockDescription { full_text = T.pack $ text b, color = T.pack <$> textColor b }
