{-|
Module      : Hi3Status.Block
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental

Blocks are the main components of a status line. A block is any instance of the
'Block' class.

-}

module Hi3Status.Block (
    -- * Class
    Block (..),
    -- * Monad
    BlockM (),
    pushBlockDescription,
    waitForUpdateSignal,
    getUpdater,
    -- * Descriptions
    BlockDescription (..),
    emptyBlockDescription
    ) where

import Hi3Status.Block.Internal 

import Control.Concurrent.Chan
import Control.Concurrent.MVar

-- | Push a new block description to the status line.
pushBlockDescription :: BlockDescription -> BlockM ()
pushBlockDescription bd = BlockM $ \p _ c -> writeChan c . BlockUpdate p $ bd

-- | Wait until an update is required.
waitForUpdateSignal :: BlockM ()
waitForUpdateSignal = BlockM $ \_ u _ -> takeMVar u >> return ()

-- | Return the updater for this block, i.e. a value of the type @IO ()@ that
-- when run will request that the block be updated. This is useful when, for
-- example, we desire to set an update timer internal to the block itself:
-- 
-- @
-- updateInFiveSeconds :: BlockM ()
-- updateInFiveSeconds = do
--     updater <- getUpdater
--     liftIO $ forkIO $ do
--         threadDelay 5000000
--         updater
-- @
getUpdater :: BlockM (IO ())
getUpdater = BlockM $ \_ u _ -> return $ update u


class Block a where
    -- | This function takes in a block, and returns a value of type 
    -- @BlockM ()@, which is executed in its own thread when the status line
    -- begins.
    runBlock :: a -> BlockM ()


