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
import XMonad.Layout.Grid
import XMonad.Layout.Cross

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
    "freeplane", "wicd-gtk", "xournalpp",
    "gimp", "kate", "pcmanfm", "texmaker",
    "pavucontrol", "evince", "blender", "dia", "discord", "obs", "typora", "skypeforlinux", "zoom",
    "/opt/Buttercup/buttercup-desktop", "~/HDD/apps/Telegram/Telegram"
    ]

mySystemCommands = [
    "scrot -d 1",
    "gnome-screensaver-command -l",
    "scrot -d 5",
    "screenkey",
    "killall screenkey",
    "gromit-mpx --clear",
    "shutdown -h now",
    "shutdown -r now"
    ]

-- Additional keybindings
myKeys mm = [((mm, xK_g), goToSelected defaultGSConfig),
        ((mm, xK_s), spawnSelected defaultGSConfig myPrograms),
        ((mm, xK_v), spawnSelected defaultGSConfig mySystemCommands),
        ((mm, xK_p), shellPrompt defaultXPConfig),
	((mod1Mask, xK_space), spawn $ "python /home/bakirillov/.xmonad/layout.py")]

-- Xmonad layouts
-- TODO: Play with TopicSpaces. Would be fun to use
myLayouts = simpleCross ||| simpleTabbed ||| Full ||| spacing_grid ||| spacing_tile ||| Mirror spacing_tile ||| progtile 
  where
     spacing = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True
     spacing_tile = spacing $ tile
     tile   = Tall n d r
     n = 1
     r   = 2/3
     d   = 4/100
     spacing_grid = spacing $ Grid
     progtile = spacing $ (Tall 1 d $ (1/2))
     

-- Manage hooks
myManageHooks = composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "feh" --> doFloat
    , className =? "gimp" --> doFloat
    , isFullscreen --> doFullFloat
    ]
    

main = do
  spawn "lux -s 70%"
  spawn "expressvpn connect"
  spawn "gromit-mpx"
  spawn "compton --backend glx --xrender-sync --xrender-sync-fence -fcCz -l -17 -t -17"
  spawn "xrandr --output DP-0 --left-of HDMI-0"
  spawn "feh --bg-center ~/.xmonad/wallpaper.jpg" -- Copy your wallpaper to wallpaper.jpg before start
  xmproc <- spawnPipe "/home/bakirillov/.local/bin/xmobar /home/bakirillov/.xmonad/.xmobarrc"
  xmonad $ defaultConfig {
    modMask = mod4Mask, -- Windows key
    normalBorderColor = "black",
    focusedBorderColor = "blue",
    terminal = "terminator",
    focusFollowsMouse = False,
    manageHook = manageDocks <+> myManageHooks,
    layoutHook = avoidStruts $ myLayouts,
    startupHook = setWMName "LG3D",
    logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc,
                          ppTitle = xmobarColor "blue" "" . shorten 50
                        }
    } `additionalKeys` (myKeys mod4Mask)
