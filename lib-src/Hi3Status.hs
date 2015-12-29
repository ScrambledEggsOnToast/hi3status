{-|
Module      : Hi3Status
License     : MIT
Maintainer  : Josh Kirklin (jjvk2@cam.ac.uk)
Stability   : experimental

Hi3status is a compact, lightweight, responsive and highly configurable status line for i3bar. 

The hi3status package comes in three parts:

    * @hi3status@ the library contains all of the code necessary for running a status line, as well as some basic components (or blocks) that can be used to make up a status line.
    * @hi3status@ the program is responsible for actually running the status line.
    * @hi3status-ctl@ is a small program which can be used to trigger updates to the status line.

= hi3status configuration

The hi3status configuration file is written in Haskell and is located at @~\/.config\/hi3status\/hi3status.hs@. A brief example of this file is given below:

@
{-# LANGUAGE OverloadedStrings #-}
import Hi3Status
import Hi3Status.Blocks.StaticText
import Hi3Status.Blocks.Clock

myBlocks = 
  [ "message" %% StaticTextBlock "Welcome to hi3status" Nothing
  , "time"    %% ClockBlock "%H:%M:%S %d\/%m\/%Y"
  ]

main = hi3status myBlocks
@

The main structural element of a hi3status status line are its blocks, which are defined to be values of types which are instances of the 'Block' class. For example in the above, 'Hi3Status.Blocks.StaticText.StaticTextBlock' is a block that displays a piece of unchanging text with an optional given color, and 'Hi3Status.Blocks.Clock.ClockBlock' is a block that displays a clock with the given formatting.

Each block provided to hi3status must have a unique name associated with it. To do this we form a 'BlocksEntry' using the '%%' operator: 

@
aBlockEntry = "aUniqueName" %% aBlock
@

Finally we combine these 'BlocksEntry's into a single value of the type 'Blocks', which is just a type synonym for @[BlocksEntry]@. We then wire up the @main@ function of the configuration file to this value using 'hi3status':

@
main = hi3status myBlocks
@

= i3bar configuration

In order to use hi3status as the status line for i3bar, make sure the following is present in your @.i3\/config@:

@
bar {
    status_command hi3status
    ...
}
@

= hi3status-ctl

The hi3status-ctl program is used to trigger updates to hi3status. For example, suppose we have a block which displays the current volume of the speakers. It would be desireable to have hi3status update this block when the volume changes. We would thus take the appropriate action such that the following is run every time the volume changes:

@
hi3status-ctl --name=volume
@

Note that the name specified above is the unique name given in the 'BlocksEntry' for the appropriate block.

-}
module Hi3Status (
  -- * Main entry point
    hi3status,
    startStatusLine,
  -- * Blocks
    Blocks (..),
    BlocksEntry,
    (%%)
    ) where

import Hi3Status.StatusLine
import Hi3Status.Block
import Hi3Status.Blocks.StaticText

import qualified Config.Dyre as Dyre
import Data.String

showError :: Blocks -> String -> Blocks
showError bs e = "error" %% StaticTextBlock (fromString (head . lines $ e)) Nothing : bs

realMain bs = startStatusLine bs

-- | Run hi3status. Usually this will be used in the configuration file as follows: @main = hi3status myBlocks@.
hi3status = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "hi3status"
    , Dyre.realMain = realMain
    , Dyre.showError = showError
    }

