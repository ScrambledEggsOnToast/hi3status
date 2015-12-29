{-|
Module      : Hi3Status.Blocks.Weather
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental
-}
module Hi3Status.Blocks.Weather (
    WeatherBlock (..)
    ) where

import Hi3Status.Block
import Hi3Status.Block.Util

import System.Process
import System.Exit
import Data.List
import qualified Data.Text as T
import Control.Monad.IO.Class

data Conditions = ClearDay | ClearNight | Rain | Cloudy

-- | A block to display the weather. Requires @weather@ and @sunwait@.
data WeatherBlock = WeatherBlock  {
    -- | The weather format.
    --
    -- *@{icon}@ = An icon specifying conditions.
    -- *@{temp}@ = The temperature.
    format :: String, 
    -- | @{icon}@ when it's clear in the day.
    clearDayText :: String, 
    -- | @{icon}@ when it's clear at night.
    clearNightText :: String, 
    -- | @{icon}@ when it's raining.
    rainText :: String, 
    -- | @{icon}@ when it's cloudy.
    cloudyText :: String, 
    -- | True for Celsius, False for Farenheit.
    celsius :: Bool,
    -- | Location code.
    location :: String,
    -- | Latitude.
    latitude :: String,
    -- | Longitude.
    longitude :: String
    }

weather :: String -> Bool -> String -> String -> IO (Conditions, Int)
weather loc celsius long lat = do
    w <- readProcess "weather" (if celsius then ["-m",loc] else [loc]) ""
    temp <- readProcess "grep" ["emperature"] w >>= readProcess "awk" ["{print $2}"]
    con <- if "cloud" `isInfixOf` w then return Cloudy else if "rain" `isInfixOf` w then return Rain
        else do
            (daynight,_,_) <- readProcessWithExitCode "sunwait" ["poll", long, lat] ""
            case daynight of
                ExitFailure 3 -> return ClearNight
                otherwise -> return ClearDay
    return (con,read temp)

instance Block WeatherBlock where
    runBlock b = periodic_ 1000000000 $ do
        (c,t) <- liftIO $ weather (location b) (celsius b) (longitude b) (latitude b)
        let icon = case c of
                ClearDay -> clearDayText b
                ClearNight -> clearNightText b
                Rain -> rainText b
                Cloudy -> cloudyText b
        pushBlockDescription $ emptyBlockDescription { full_text = formatText [("icon",icon),("temp", show t)] (format b) }
