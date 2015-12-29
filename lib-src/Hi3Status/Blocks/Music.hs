{-|
Module      : Hi3Status.Blocks.Music
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental
-}
module Hi3Status.Blocks.Music
  ( MusicBlock (..)
  ) where

import Hi3Status.Block
import Hi3Status.Block.Util

import qualified Data.Text as T
import Data.List (isInfixOf)
import Data.Maybe (fromJust)

import Control.Monad.IO.Class
import System.Process
import System.Exit

-- | Currently playing music. Uses @playerctl@.
data MusicBlock = MusicBlock {
    -- | The format of the displayed text.
    --
    -- *@{playpause}@ = Play/pause icon.
    -- *@{artist}@ = Artist.
    -- *@{track}@ = Track.
    -- *@{album}@ = Album.
    format :: String,
    -- | The maximum length of the displayed text.
    maxLength :: Maybe Int,
    -- | The icon to display when playing.
    playIcon :: String,
    -- | The icon to display when paused.
    pauseIcon :: String
    }

instance Block MusicBlock where
    runBlock b = periodic 1000000 $ do
        p <- liftIO playing
        if p == Nothing 
            then pushBlockDescription $ emptyBlockDescription
            else do
                (artist, track, album) <- liftIO metadata
                pushBlockDescription $ emptyBlockDescription 
                    { full_text = T.pack . (flip maxLengthText (maxLength b)) . T.unpack $ formatText 
                        [ ("artist", artist)
                        , ("track", track)
                        , ("album", album)
                        , ("playpause", if fromJust p then playIcon b else pauseIcon b)
                        ] (format b)
                    }

playing :: IO (Maybe Bool)
playing = do
    (e,p,_) <- readProcessWithExitCode "playerctl" ["status"] ""
    if e == ExitFailure 1 then return Nothing
        else if "laying" `isInfixOf` p then return (Just True)
            else if "ause" `isInfixOf` p then return (Just False)
                else return Nothing

metadata :: IO (String, String, String)
metadata = do
    artist <- filter (/='\n') <$> readProcess "playerctl" ["metadata","artist"] ""
    track <- filter (/='\n') <$> readProcess "playerctl" ["metadata","title"] ""
    album <- filter (/='\n') <$> readProcess "playerctl" ["metadata","album"] ""
    return (artist, track, album)
