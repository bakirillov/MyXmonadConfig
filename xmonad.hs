{-# LANGUAGE NoMonomorphismRestriction #-}

import XMonad

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- Layouts
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing

-- Actions
import XMonad.Actions.GridSelect

-- Utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Shell

-- Misc
import System.IO
import Control.Concurrent.MVar

-- Programs list for GridSelect
-- TODO: play with GSConfig
myPrograms = ["firefox", "localc", "lowriter",
    "freeplane", "wicd-gtk",
    "gimp", "kate", "pcmanfm", "texmaker",
    "pavucontrol", "evince", "blender", "dia", "discord", "obs",
    "/opt/Buttercup/buttercup-desktop", "~/HDD/apps/Telegram/Telegram"
    ]

mySystemCommands = [
    "gnome-screensaver-command -l",
    "scrot -d 5",
    "scrot -d 1"
    ]

-- Additional keybindings
myKeys mm = [((mm, xK_g), goToSelected defaultGSConfig),
        ((mm, xK_s), spawnSelected defaultGSConfig myPrograms),
        ((mm, xK_v), spawnSelected defaultGSConfig mySystemCommands),
        ((mm, xK_p), shellPrompt defaultXPConfig),
	((mod1Mask, xK_space), spawn $ "python /home/bakirillov/.xmonad/layout.py")]

-- Xmonad layouts
-- TODO: Play with TopicSpaces. Would be fun to use
myLayouts = spacing_tile ||| Mirror spacing_tile ||| Full ||| simpleTabbed
  where
     spacing_tile = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ tile
     tile   = Tall n d r
     n = 1
     r   = 2/3
     d   = 4/100
     progtile = Tall 1 d $ (1/2)

-- Manage hooks
myManageHooks = composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "feh" --> doFloat
    , className =? "gimp" --> doFloat
    , isFullscreen --> doFullFloat
    ]

main = do
  spawn "lux -s 60%"
  spawn "expressvpn connect"
  spawn "xrandr --output DP-0 --right-of HDMI-0"
  spawn "feh --bg-center ~/.xmonad/wallpaper.jpg" -- Copy your wallpaper to wallpaper.jpg before start
  spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --height 12 --transparent true --tint 0x000000 "
  xmproc <- spawnPipe "/home/bakirillov/.local/bin/xmobar /home/bakirillov/.xmonad/.xmobarrc"
  xmonad $ defaultConfig {
    modMask = mod4Mask, -- Windows key
    normalBorderColor = "black",
    focusedBorderColor = "blue",
    terminal = "terminator",
    manageHook = manageDocks <+> myManageHooks,
    layoutHook = avoidStruts $ myLayouts,
    startupHook = setWMName "LG3D",
    logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc,
                          ppTitle = xmobarColor "blue" "" . shorten 50
                        }
    } `additionalKeys` (myKeys mod4Mask)
