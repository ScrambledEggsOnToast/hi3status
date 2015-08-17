{-# LANGUAGE GADTs, OverloadedStrings #-}

module Hi3Status.Block 
  ( Block (..)
  , BlockM ()
  , pushBlockDescription
  , waitForUpdateSignal
  , getUpdater
  , BlockDescription (..)
  , emptyBlockDescription
  ) where

import Hi3Status.Block.Internal 

import qualified Data.Text as T
import Data.String
import qualified Data.Aeson as A
import Data.Aeson ((.=))

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Control.Monad.IO.Class

pushBlockDescription :: BlockDescription -> BlockM ()
pushBlockDescription bd = BlockM $ \p _ c -> writeChan c . BlockUpdate p $ bd

waitForUpdateSignal :: BlockM ()
waitForUpdateSignal = BlockM $ \_ u _ -> takeMVar u >> return ()

getUpdater :: BlockM (IO ())
getUpdater = BlockM $ \_ u _ -> return $ update u

class Block a where
    runBlock :: a -> BlockM ()


