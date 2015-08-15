{-# LANGUAGE GADTs, TypeOperators, OverloadedStrings, UnicodeSyntax #-}

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import System.IO

import Data.Time.Clock
import Data.Time.Format

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B

import qualified Data.Aeson as A
import Data.Aeson ((.=))

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import DBus
import DBus.Client

import Data.String

import Block
import StatusLine
import Block.StaticText
import Block.Clock
import Block.Counter

main = do
    hSetBuffering stdout LineBuffering -- set buffering correctly
    startStatusLine sampleBlocks
  where
    sampleBlocks = "hello" %% staticTextBlock "Hello \61441" Nothing
                :& "time" %% ClockBlock
                :& "counter" %% CounterBlock 0
                :& EndBlock
