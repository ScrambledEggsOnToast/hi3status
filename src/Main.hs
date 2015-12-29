import System.Environment
import System.Console.GetOpt

import Hi3Status
import Hi3Status.Blocks.Clock
import Hi3Status.Blocks.Volume
import Hi3Status.Blocks.Window

data Configuration = Configuration { preset :: Bool, fontawesome :: Bool }
defaultConfiguration = Configuration False False

options :: [OptDescr (Configuration -> Configuration)]
options =
    [ Option ['p'] ["preset"] (NoArg preset') "use preset configuration (window title, volume, time, date)"
    , Option ['f'] ["fontawesome"] (NoArg fontawesome') "use FontAwesome icons in preset configuration"
    ]

preset' :: Configuration -> Configuration
preset' c = c { preset = True }

fontawesome' :: Configuration -> Configuration
fontawesome' c = c { fontawesome = True }

main = do
    args <- getArgs
    case getOpt Permute options args of
        (cfs, _, []) -> do
            let c = foldr ($) defaultConfiguration cfs
            if preset c then startStatusLine (presetBlocks (fontawesome c)) else hi3status []
        _ -> putStrLn (usageInfo "hi3status - Haskell status line for i3bar." options)

presetBlocks :: Bool -> Blocks
presetBlocks True = 
  [ "window"  %% WindowBlock "\61474 {title}" (Just 100)
  , "volume"  %% VolumeBlock "{icon} {perc}%" "{icon}" "\61480" "\61479" "\61478" "default" "Master"
  , "time"    %% ClockBlock "\61463 %H:%M:%S"
  , "date"    %% ClockBlock "\61555 %m/%d/%Y"
  ]
presetBlocks False = 
  [ "window"  %% WindowBlock "{title}" (Just 100)
  , "volume"  %% VolumeBlock "{perc}%" "mute" "" "" "" "default" "Master"
  , "time"    %% ClockBlock "%H:%M:%S"
  , "date"    %% ClockBlock "%m/%d/%Y"
  ]

