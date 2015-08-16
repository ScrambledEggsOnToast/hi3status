module Hi3Status.Blocks.StaticText where

import Hi3Status.Block

import qualified Data.Text as T

data StaticTextBlock = StaticTextBlock { text :: String, textColor :: (Maybe String) }

instance Block StaticTextBlock where
    runBlock b = do
        pushBlockDescription $ emptyBlockDescription { full_text = T.pack $ text b, color = T.pack <$> textColor b }
