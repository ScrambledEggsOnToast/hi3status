{-|
Module      : Hi3Status.Blocks.Window
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental
-}
{-# LANGUAGE OverloadedStrings #-}

module Hi3Status.Blocks.Window (
    WindowBlock (..)
    ) where

import Hi3Status.Block

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import System.Process
import qualified Data.ByteString.Lazy as BS
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Int (Int64)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.Aeson.Types ((.:))
import Data.String
import qualified Data.Text as T
import Control.Monad.IO.Class

data MessageType = Command | Workspaces | Subscribe | Outputs | Tree | Marks | BarConfig | Version deriving (Enum, Show)

-- | This block displays the title of the currently focused window.
data WindowBlock = WindowBlock {
    -- | The format of the displayed text.
    --
    -- * @{title}@ = Window title.
    format :: String,
    -- | The maximum length of the displayed title.
    maxLength :: (Maybe Int) 
    }

instance Block WindowBlock where
    runBlock b = do
        soc <- liftIO $ socket AF_UNIX Stream 0
        addr <- liftIO socketAddr
        liftIO $ connect soc addr
        liftIO $ sendMessage soc Subscribe "[\"window\"]"
        go soc
        liftIO $ sClose soc
      where
        go soc = do
            Just (_,p) <- liftIO $ recieveReply soc
            let titleResult = AT.parse focusParser p
            case titleResult of
                AT.Error _ -> return ()
                AT.Success title -> pushBlockDescription $ emptyBlockDescription { full_text = title }
            go soc

sendMessage :: Socket -> MessageType -> BS.ByteString -> IO Int64
sendMessage soc messagetype payload = send soc msg
  where
    msg = runPut $ do
        putByteString "i3-ipc"
        putWord32host $ fromIntegral (BS.length payload)
        putWord32host $ fromIntegral (fromEnum messagetype)
        putLazyByteString payload

recieveReply :: Socket -> IO (Maybe (MessageType, A.Value))
recieveReply soc = do
    magic <- recv soc 6
    if magic == "i3-ipc" 
        then do
            length <- fromIntegral . decode32 <$> recv soc 4
            messageType <- toEnum . fromIntegral . decode32 <$> recv soc 4
            payload <- recv soc length
            return $ do 
                payloadValue <- A.decode payload
                return (messageType, payloadValue)
        else return Nothing
  where
    decode32 = runGet getWord32host 

socketAddr :: IO SockAddr
socketAddr = do
    s <- readProcess "i3" ["--get-socketpath"] ""
    return $ SockAddrUnix $ filter (/='\n') s

focusParser :: A.Value -> AT.Parser T.Text
focusParser = AT.withObject "Window event not object" $ \o -> do
    AT.String t <- o .: "change"
    if (t == "focus" || t == "title")
        then do
            AT.Object c <- o .: "container" 
            AT.Object pr <- c .: "window_properties"
            AT.String title <- pr .: "title"
            return title
        else AT.typeMismatch "Window event not focus change" (AT.Object o)

