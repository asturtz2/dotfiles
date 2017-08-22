import System.IO

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
-- import XMonad.Layout.ResizableTile

import XMonad.Util.Run(spawnPipe, runInTerm)
import XMonad.Util.EZConfig(additionalKeys)

import XMonad.Actions.WindowGo
import XMonad.Actions.SpawnOn

import XMonad.Prompt

main = do
    bar <- polybar
    xmonad config

polybar = spawn "polybar main"

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
    $ tiled ||| Mirror tiled ||| simplestFloat
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
spaces =
    [ "dev"
    , "web"
    , "mail"
    , "media"
    , "chat"
    , "office"
    , "log"
    ]

-- Keymappings
keybinds :: [((KeyMask, KeySym), X ())]
keybinds =
    [ minimize
    , restore
    , toggleFullScreen
    , qutebrowser
    , zathura
    , rtv
    , vim
    , ranger
    , weechat
    , mail
    , htop
    , rofiShow
    , rofiRun
    , reload
    ]
  where
    --Tuples of keys/masks and X actions
    reload   = ((mod1Mask, xK_q), spawn "reload-xmonad")
    minimize = ((mod1Mask, xK_m), withFocused minimizeWindow)
    restore  = ((mod1Mask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
    rofiShow = ((mod1Mask, xK_s), spawn "rofi -show window")
    rofiRun  = ((mod1Mask, xK_r), spawn "rofi -show run")
    qutebrowser = ((mod1Mask .|. shiftMask, xK_b), spawn "qutebrowser")
    zathura = ((mod1Mask .|. shiftMask, xK_z), spawn "zathura")
    rtv     = ((mod1Mask .|. shiftMask, xK_r), runInTerm "-title rtv" "rtv")
    vim     = ((mod1Mask .|. shiftMask, xK_v), runInTerm "-title vim" "nvim")
    htop    = ((mod1Mask .|. shiftMask, xK_h), runInTerm "-title htop" "htop")
    toggleFullScreen = ((mod1Mask, xK_f), sendMessage $ Toggle FULL)
    ranger  =
      ( (mod1Mask .|. shiftMask, xK_t)
      , runInTerm "-title ranger" "env EDITOR=nvim ranger"
      )
    weechat =
      ( (mod1Mask .|. shiftMask, xK_i)
      , runInTerm "-title weechat" "weechat"
      )
    mail    =
      ( (mod1Mask .|. shiftMask, xK_m)
      , runInTerm "-title mail" "sup-mail"
      )
