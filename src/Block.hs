{-# LANGUAGE GADTs, OverloadedStrings #-}

module Block where

import qualified Data.Text as T

import qualified Data.Aeson as A
import Data.Aeson ((.=))

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Control.Monad.IO.Class

data BlockAlign = AlignLeft | AlignCenter | AlignRight 

instance A.ToJSON BlockAlign where
    toJSON AlignLeft = A.String "left"
    toJSON AlignCenter = A.String "center"
    toJSON AlignRight = A.String "right"

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

data BlockUpdate = BlockUpdate Int BlockDescription

data UpdateSignal = UpdateSignal

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

pushBlockDescription :: BlockDescription -> BlockM ()
pushBlockDescription bd = BlockM $ \p _ c -> writeChan c . BlockUpdate p $ bd

waitForUpdateSignal :: BlockM ()
waitForUpdateSignal = BlockM $ \_ u _ -> takeMVar u >> return ()

getUpdater :: BlockM (IO ())
getUpdater = BlockM $ \_ u _ -> return (tryPutMVar u UpdateSignal >> return ())

class Block a where
    runBlock :: a -> BlockM ()

data BlocksEntry a = BlocksEntry String a

(%%) :: String -> a -> BlocksEntry a
(%%) = BlocksEntry
infixl 7 %%

data Blocks where
    EndBlock :: Blocks
    (:&) :: Block a => BlocksEntry a -> Blocks -> Blocks
infixr 6 :&

blockCount :: Blocks -> Int
blockCount EndBlock = 0
blockCount (b :& bs) = 1 + blockCount bs
