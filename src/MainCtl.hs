{-# LANGUAGE OverloadedStrings #-}

import DBus
import DBus.Client
import Data.String

import System.Console.GetOpt
import System.Environment

data CtlOption = BlockName String deriving Show

data CtlAction = UpdateAll | Update String

callMethod :: String -> String -> IO ()
callMethod o m = do
    cl <- connectSession
    let c = (methodCall (fromString o) "org.i3wm.hi3status" (fromString m)) { methodCallDestination = Just "org.i3wm.hi3status" }
    callNoReply cl c
    return ()

act :: CtlAction -> IO ()
act UpdateAll = callMethod "/" "UpdateAll"
act (Update name) = callMethod ("/" ++ name) "Update"

modifyAction :: CtlOption -> CtlAction -> CtlAction
modifyAction (BlockName name) UpdateAll = Update name
modifyAction (BlockName name) (Update _) = Update name

blockNameOption = 
    Option ['n'] ["name"]
        (OptArg (\ms -> case ms of
            Nothing -> BlockName ""
            Just s -> BlockName s) "STRING") "Name of block"

main = do
    args <- getArgs
    let (opts,_,err) = getOpt RequireOrder [blockNameOption] args
    case err of
        [] -> do
            let modifications = map modifyAction opts
                action = foldl (flip ($)) UpdateAll modifications
            act action
        _ -> do
            putStr $ usageInfo "hi3status-ctl" [blockNameOption]
            mapM_ putStrLn err
    
