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
import Hi3Status.Block.Util

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
import Data.Bits (clearBit, testBit)
import Control.Monad.IO.Class
import Control.Monad (when)

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
        liftIO $ sendMessage soc Subscribe "[\"window\",\"workspace\"]"
        go soc
        liftIO $ sClose soc
      where
        go soc = do
            Just (t,p) <- liftIO $ recieveReply soc
            when (t == Event Window || t == Event Workspace) $ do
                let titleResult = case t of
                        Event Window -> AT.parse windowParser p
                        Event Workspace -> AT.parse workspaceParser p
                case titleResult of
                    AT.Error _ -> return ()
                    AT.Success (Just title) -> do
                        let t = case (maxLength b) of
                                Nothing -> T.unpack title
                                Just n -> (take n $ T.unpack title) ++ (if n < T.length title then "..." else "")
                        pushBlockDescription $ emptyBlockDescription { full_text = formatText [("title", t)] (format b) }
                    otherwise -> pushBlockDescription emptyBlockDescription { full_text = "" }
            go soc

data MessageType = Command | Workspaces | Subscribe | Outputs | Tree | Marks | BarConfig | Version deriving (Enum, Show, Eq)
data EventType = Workspace | Output | Mode | Window | BarconfigUpdate | Binding deriving (Enum, Show, Eq)
data ReplyType = Message MessageType | Event EventType deriving (Show, Eq)

sendMessage :: Socket -> MessageType -> BS.ByteString -> IO Int64
sendMessage soc messagetype payload = send soc msg
  where
    msg = runPut $ do
        putByteString "i3-ipc"
        putWord32host $ fromIntegral (BS.length payload)
        putWord32host $ fromIntegral (fromEnum messagetype)
        putLazyByteString payload

recieveReply :: Socket -> IO (Maybe (ReplyType, A.Value))
recieveReply soc = do
    magic <- recv soc 6
    if magic == "i3-ipc" 
        then do
            length <- fromIntegral . decode32 <$> recv soc 4
            replyTypeB <- fromIntegral . decode32 <$> recv soc 4
            payload <- recv soc length
            let replyType = if testBit replyTypeB 31 then Event (toEnum (replyTypeB `clearBit` 31)) else Message (toEnum replyTypeB)
            return $ do 
                payloadValue <- A.decode payload
                return (replyType, payloadValue)
        else return Nothing
  where
    decode32 = runGet getWord32host 

socketAddr :: IO SockAddr
socketAddr = do
    s <- readProcess "i3" ["--get-socketpath"] ""
    return $ SockAddrUnix $ filter (/='\n') s

windowParser :: A.Value -> AT.Parser (Maybe T.Text)
windowParser = AT.withObject "Window event object" $ \o -> do
    AT.String t <- o .: "change"
    if (t == "focus" || t == "title")
        then do
            AT.Object c <- o .: "container" 
            AT.Object pr <- c .: "window_properties"
            AT.String title <- pr .: "title"
            return (Just title)
        else AT.typeMismatch "Window event focus change" (AT.Object o)

workspaceParser :: A.Value -> AT.Parser (Maybe T.Text)
workspaceParser = AT.withObject "Workspace event object" $ \o -> do
    AT.String t <- o .: "change"
    if t == "focus"
        then do
            AT.Object curr <- o .: "current"
            n <- curr .: "nodes"
            fn <- curr .: "floating_nodes"
            if (n == AT.emptyArray && fn == AT.emptyArray) then return Nothing else AT.typeMismatch "Workspace empty" (AT.Object o)
        else AT.typeMismatch "Workspace event focus change" (AT.Object o)
