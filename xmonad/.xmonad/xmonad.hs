import System.IO

import Prelude hiding (mod)
import XMonad hiding (config)

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.EqualSpacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Minimize
import XMonad.Layout.Tabbed
-- import XMonad.Layout.ResizableTile

import XMonad.Util.Run(spawnPipe, runInTerm)
import XMonad.Util.EZConfig(additionalKeys)

import XMonad.Actions.WindowGo
import XMonad.Actions.SpawnOn

import XMonad.Prompt

main = do
    bar      <- polybar
    -- player   <- mopidy
    -- monitors <- mons
    xmonad config

polybar = spawn "polybar --reload main"
-- mopidy  = spawn "mopidy"

config = ewmh . docks $ def
    { terminal           = "urxvt"
    , borderWidth        = 4
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#8A745E"
    , layoutHook         = layout
    , manageHook         = manager
    , handleEventHook    = events
    , logHook            = logger
    , workspaces         = spaces
    } `additionalKeys` keybinds

-- Layouts
-- layout :: l Window
layout = id
    . equalSpacing gapWidth gapShrink mult minWidth
    . avoidStruts
    . mkToggle (single FULL)
    . minimize
    $ tiled ||| Mirror tiled ||| simpleTabbed ||| simplestFloat
  where
    gapWidth  = 15
    gapShrink = 0
    mult      = 0
    minWidth  = 1
    tiled     = Tall nmaster delta ratio
    nmaster   = 1
    delta     = 3/100
    ratio     = 1/2

manager = manageDocks <+> manageWorkspaces <+> manageHook defaultConfig
manageWorkspaces = composeAll
    [ className =? "firefox"    --> doShift "3:web"
    ]

logger = ewmhDesktopsLogHook

events = handleEventHook def <+> fullscreenEventHook

spaces :: [String]
spaces = [ "dev" , "web" , "mail" , "media" , "chat" , "office" , "log" ]

mod   = mod1Mask
shift = shiftMask

-- Keymappings
keybinds :: [((KeyMask, KeySym), X ())]
keybinds =
    [ minimize
    , restore
    , fullscreen
    , qutebrowser
    , zathura
    , ranger
    , weechat
    , mail
    , rofiShow
    , rofiRun
    ]
  where
    --Tuples of keys/masks and X actions
    rofiShow     = ((mod, xK_s),           spawn "rofi -show window")
    rofiRun      = ((mod, xK_r),           spawn "rofi -show run")
    -- reload       = ((mod, xK_q),           spawn "reload-xmonad")
    fullscreen   = ((mod, xK_f),           sendMessage $ Toggle FULL)
    minimize     = ((mod, xK_m),           withFocused minimizeWindow)
    qutebrowser  = ((mod .|. shift, xK_b), spawn "qutebrowser")
    zathura      = ((mod .|. shift, xK_z), spawn "zathura")
    restore      = ((mod .|. shift, xK_m), sendMessage RestoreNextMinimizedWin)
    ranger  =
      ( (mod .|. shift, xK_t)
      , runInTerm "-title ranger" "env EDITOR=nvim ranger"
      )
    weechat =
      ( (mod .|. shift, xK_i)
      , runInTerm "-title weechat" "weechat"
      )
    mail    =
      ( (mod .|. shift, xK_m)
      , runInTerm "-title mail" "sup-mail"
      )
