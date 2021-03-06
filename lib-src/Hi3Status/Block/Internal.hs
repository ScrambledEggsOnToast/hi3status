{-|
Module      : Hi3Status.Block.Internal
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental
-}
{-# LANGUAGE OverloadedStrings #-}
module Hi3Status.Block.Internal where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Control.Monad.IO.Class

import qualified Data.Text as T
import Data.String
import qualified Data.Aeson as A
import Data.Aeson ((.=))

data BlockUpdate = BlockUpdate 
  { blockUpdatePosition :: Int
  , blockUpdateDescription :: BlockDescription
  }

data UpdateSignal = UpdateSignal

-- | 'BlockM' is a monad responsible for controlling the operation of a block. Each block has two lines of communication with the main status line:
-- 
-- * The status line can tell the block that it needs updating.
-- * The block can push updates in the form of 'BlockDescription's to the status line, which will then be processed and submitted to i3bar.
data BlockM a = BlockM 
  { runBlockM :: Int -> MVar UpdateSignal -> Chan BlockUpdate -> IO a }

instance Functor BlockM where
    fmap f blockm = BlockM $ \p u c -> fmap f $ runBlockM blockm p u c

instance Applicative BlockM where
    pure a = BlockM $ \_ _ _ -> return a
    (BlockM f) <*> (BlockM a) = BlockM $ \p u c -> (f p u c) <*> (a p u c)

instance Monad BlockM where
    BlockM a >>= m = BlockM $ \p u c -> do
        a' <- a p u c
        runBlockM (m a') p u c
    return = pure
    fail e = BlockM $ \_ _ _ -> fail e

instance MonadIO BlockM where
    liftIO io = BlockM $ \_ _ _ -> io

update u = tryPutMVar u UpdateSignal >> return ()

data BlockAlign = AlignLeft | AlignCenter | AlignRight 

instance A.ToJSON BlockAlign where
    toJSON AlignLeft = A.String "left"
    toJSON AlignCenter = A.String "center"
    toJSON AlignRight = A.String "right"

-- | A 'BlockDescription' contains everything needed by i3bar to properly render a block. This includes things like the text of a block, and the color of the text. For more information about each field, see the i3bar protocol at <http://i3wm.org/docs/i3bar-protocol.html>.
data BlockDescription = BlockDescription
  { full_text :: T.Text
  , short_text :: Maybe T.Text
  , color :: Maybe T.Text
  , min_width :: Maybe Int
  , align :: Maybe BlockAlign
  , name :: Maybe T.Text
  , instanc :: Maybe T.Text
  , urgent :: Maybe Bool
  , separator :: Maybe Bool
  , separator_block_width :: Maybe Int
  , markup :: Maybe T.Text
  }

instance A.ToJSON BlockDescription where
    toJSON d = A.object . filter ((/= A.Null) . snd) $
      [ "full_text" .= full_text d
      , "short_text" .= short_text d
      , "color" .= color d
      , "min_width" .= min_width d
      , "align" .= align d
      , "name" .= name d
      , "instance" .= instanc d
      , "urgent" .= urgent d
      , "separator" .= separator d
      , "separator_block_width" .= separator_block_width d
      , "markup" .= markup d
      ]

instance IsString BlockDescription where
    fromString str = emptyBlockDescription { full_text = fromString str }

-- | An empty block description, i.e. one with @full_text = ""@, and everything else equal to @Nothing@.
emptyBlockDescription =
    BlockDescription 
        ""
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
