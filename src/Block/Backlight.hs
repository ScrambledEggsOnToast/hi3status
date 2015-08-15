module Block.Backlight where

import Block
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import System.Process
import System.Exit
import GHC.IO.Handle

import Control.Monad.IO.Class

import Data.Text.Format
import Data.String

data BacklightBlock = BacklightBlock String

instance Block BacklightBlock where
    runBlock (BacklightBlock f) = onUpdate $ do
        perc <- liftIO getBacklight
        let s = format (fromString f) (Only $ fixed 0 perc)
        pushBlockDescription $ emptyBlockDescription { full_text = TL.toStrict s }

getBacklight :: IO Float
getBacklight = do
    (_, mo, _, p) <- createProcess $ (proc "xbacklight" []) { std_out = CreatePipe }
    case mo of
        Nothing -> error "xbacklight error"
        Just o -> do
            s <- hGetContents o
            return $ read s
