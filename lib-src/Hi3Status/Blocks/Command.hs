{-|
Module      : Hi3Status.Blocks.Command
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental
-}
module Hi3Status.Blocks.Command (
    CommandBlock (..)
    ) where

import Hi3Status.Block
import Hi3Status.Block.Util

import qualified Data.Text as T
import System.Process
import GHC.IO.Handle
import Control.Monad.IO.Class

-- | A block that executes an arbitrary shell command and displays the output.
data CommandBlock = CommandBlock {
    -- | The command to execute.
    cmd :: String, 
    -- | The format of the displayed text.
    --
    -- *@{stdout}@ = stdout.
    -- *@{stderr}@ = stderr.
    format :: String,
    -- | An optional refresh period in microseconds.
    refresh :: Maybe Int 
    }

instance Block CommandBlock where
    runBlock b = case refresh b of
        Nothing -> onUpdate go
        Just n -> periodic n go
      where
        go = do
            (_, mhout, mherr, _) <- liftIO $ createProcess $ (shell (cmd b)) { std_out = CreatePipe, std_err = CreatePipe }
            out <- maybe (return "") (liftIO . hGetContents) mhout
            err <- maybe (return "") (liftIO . hGetContents) mherr
            pushBlockDescription $ emptyBlockDescription 
                { full_text = formatText [("stdout", concat . take 1 . lines $ out),("stderr", concat . take 1 . lines $ err)] (format b) }

