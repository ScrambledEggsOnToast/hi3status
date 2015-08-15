{-# LANGUAGE OverloadedStrings #-}

import DBus
import DBus.Client
import Data.String

callMethod :: String -> String -> IO ()
callMethod o m = do
    cl <- connectSession
    let c = (methodCall (fromString o) "org.i3wm.hi3status" (fromString m)) { methodCallDestination = Just "org.i3wm.hi3status" }
    callNoReply cl c
    return ()

main = callMethod "/" "Update"
