{-# LANGUAGE GADTs, OverloadedStrings #-}

module Block where

import qualified Data.Text as T

import qualified Data.Aeson as A
import Data.Aeson ((.=))

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

data BlockAlign = AlignLeft | AlignCenter | AlignRight deriving Show

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
  } deriving Show

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

data BlockUpdate = BlockUpdate Int BlockDescription deriving Show

data UpdateSignal = UpdateSignal

class Show a => Block a where
    runBlock :: a -> Int -> MVar UpdateSignal -> Chan BlockUpdate -> IO ()

data BlocksEntry a = BlocksEntry String a deriving Show

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

instance Show Blocks where
        show EndBlock = "EndBlock"
        show blocks = "Blocks " ++ show' blocks
          where
            show' (b :& EndBlock) = "(" ++ show b ++ ")"
            show' (b :& bs) = "(" ++ show b ++ ") " ++ show' bs
