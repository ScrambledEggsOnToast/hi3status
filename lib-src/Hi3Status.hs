module Hi3Status
  ( hi3status
  , defaultBlocks
  , Blocks (..)
  , BlocksEntry
  , (%%)
  ) where

import Hi3Status.StatusLine
import Hi3Status.Block
import Hi3Status.Blocks.StaticText

import qualified Config.Dyre as Dyre
import Data.String

defaultBlocks :: Blocks
defaultBlocks = showError EndBlock "Config file missing"

showError :: Blocks -> String -> Blocks
showError bs e = "error" %% StaticTextBlock (fromString e) Nothing :& bs

realMain bs = startStatusLine bs

hi3status = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "hi3status"
    , Dyre.realMain = realMain
    , Dyre.showError = showError
    }
