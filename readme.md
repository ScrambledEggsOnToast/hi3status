# hi3status

Hi3status is a compact, lightweight, responsive and highly configurable status line for i3bar.

https://hackage.haskell.org/package/hi3status/

## Installation

    cabal install hi3status

## Usage

### i3bar configuration

To use hi3status as the status line for i3bar, make sure the following is present in your `.i3/config`:

    bar {
        status_command hi3status
        ...
    }

If you want to use a preset bar, add `-p` to the status-command. If you want that preset to include icons from FontAwesome, add `-f`.

### hi3status configuration

To gain more control over the contents of the bar, it is necessary to write a configuration file to be located at `~/.config/hi3status/hi3status.hs`. A brief example of this file is given below:
    
    -- hi3status.hs
    import Hi3Status
    import Hi3Status.Blocks.StaticText
    import Hi3Status.Blocks.Clock

    myBlocks = 
      [ "message" %% StaticTextBlock "Welcome to hi3status" Nothing
      , "time"    %% ClockBlock "%H:%M:%S %d/%m/%Y"
      ]

    main = hi3status myBlocks

The main structural element of a hi3status status line are its blocks, which are defined to be values of types which are instances of the Block class. For example in the above, StaticTextBlock is a block that displays a piece of unchanging text with an optional given color, and ClockBlock is a block that displays a clock with the given formatting.

Each block provided to hi3status must have a unique name associated with it. To do this we form a BlocksEntry using the %% operator:

    aBlockEntry = "aUniqueName" %% aBlock

Finally we combine these BlocksEntrys into a single value of the type Blocks, which is just a type synonym for [BlocksEntry]. We then wire up the main function of the configuration file to this value using hi3status:

    main = hi3status myBlocks

### Built-in blocks

Hi3status includes the following built-in blocks:

* `Backlight`: A backlight percentage brightness indicator. Uses xbacklight as a backend.
* `Battery`: A battery indicator. Uses acpi as a backend.
* `Clock`: A customisable clock.
* `Ethernet`: An ethernet status indicator. Uses files at `/sys/class/net/`.
* `Music`: Displays currently playing music. Uses playerctl as a backend.
* `Network`: A network transfer rate indicator. Uses files at `/sys/class/net/`.
* `StaticText`: A simple block that displays a piece of static text.
* `Volume`: A block to indicate the status of the system volume. Uses amixer as a backend.
* `Weather`: A block to display the weather. Uses weather and sunwait as backends.
* `Wifi`: A wifi status indicator. Uses iwgetid as a backend.
* `Window`: This block displays the title of the currently focused window.

### Custom blocks

It is easy to write custom blocks for hi3status. Here is a brief example demonstrating a clone of the static text block:

    -- hi3status.hs
    import Hi3Status
    import Hi3Status.Blocks.Clock

    import qualified Data.Text as T

    myBlocks = 
      [ "message" %% MyStaticTextBlock "Welcome to hi3status" Nothing
      , "time"    %% ClockBlock "%H:%M:%S %d/%m/%Y"
      ]

    main = hi3status myBlocks

    data MyStaticTextBlock = MyStaticTextBlock {
        text :: String, 
        textColor :: (Maybe String) 
        }

    instance Block MyStaticTextBlock where
        runBlock b = do
            pushBlockDescription $ emptyBlockDescription { full_text = T.pack $ text b, color = T.pack <$> textColor b }

More details can be found in the haddock documentation.

### hi3status-ctl

The hi3status-ctl program is used to trigger updates to hi3status. For example, suppose we have a block which displays the current volume of the speakers. It would be desireable to have hi3status update this block when the volume changes. We would thus take the appropriate action such that the following is run every time the volume changes:

    hi3status-ctl --name=volume

For example, in our i3 configuration file we might have the following:

    bindsym XF86AudioRaiseVolume \
        exec amixer set Master unmute; \
        exec amixer set Master 5%+; \
        exec hi3status-ctl -nvolume; 
    bindsym XF86AudioLowerVolume \
        exec amixer set Master unmute; \
        exec amixer set Master 5%-; \
        exec hi3status-ctl -nvolume; 
    bindsym XF86AudioMute \
        exec amixer set Master toggle; \
        exec hi3status-ctl -nvolume; 

Note that the name specified above is the unique name given in the BlocksEntry for the appropriate block.
