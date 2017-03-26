import System.IO

import XMonad hiding (config)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.EqualSpacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace
-- import XMonad.Layout.ResizableTile

import XMonad.Util.Run(spawnPipe, runInTerm)
import XMonad.Util.EZConfig(additionalKeys)

import XMonad.Actions.WindowGo
import XMonad.Actions.SpawnOn

import XMonad.Prompt

main = do
    -- xmproc  <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    bar <- xmobar config
    xmonad bar

config = baseConfig
    { startupHook = startup
    -- , logHook            = logHook'
    , layoutHook  = layout
    , manageHook  = manager
    , workspaces  = spaces
    } `additionalKeys` keybinds

-- Basic configs
baseConfig = def
    { terminal           = "urxvt"
    , borderWidth        = 1
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#8A745E"
    }

startup :: X ()
startup = runInTerm "" "wal -i ~/wallpapers/vintage-kitchen"

-- Layouts
-- layout :: l Window
layout = id
    . equalSpacing gapWidth gapShrink mult minWidth
--    . avoidStruts
    . mkToggle (single FULL)
    $ tiled ||| Mirror tiled ||| simplestFloat
  where
    gapWidth  = 15
    gapShrink = 0
    mult      = 0
    minWidth  = 1
    tiled     = Tall nmaster delta ratio
    nmaster   = 1
    ratio     = 1/2
    delta     = 3/100

manager = manageDocks <+> manageWorkspaces <+> manageHook defaultConfig
manageWorkspaces = composeAll
    [ className =? "firefox"    --> doShift "3:web"
    ]

logHook' = dynamicLogWithPP sjanssenPP --xmobarPP
    -- { ppOutput = hPutStrLn xmproc
    -- , ppTitle  = xmobarColor "brown" "" . shorten 50
    -- }

spaces :: [String]
spaces =
    [ "1:Main"
    , "2:Dev"
    , "3:Web"
    , "4:Viewer"
    , "5:Chat"
    , "6:Mail"
    , "7:Music"
    , "8:Video"
    , "9:Scratch"
    ]

-- Keymappings
keybinds :: [((KeyMask, KeySym), X ())]
keybinds =
    [ toggleFullScreen
    , firefox
    , zathura
    , rtv
    , vim
    , ranger
    , weechat
    , mail
    , htop
    ]
  where
    --Tuples of keys/masks and X actions
    firefox = ((mod1Mask .|. shiftMask, xK_b), spawn "firefox")
    zathura = ((mod1Mask .|. shiftMask, xK_z), spawn "zathura")
    rtv     = ((mod1Mask .|. shiftMask, xK_r), runInTerm "-title rtv" "rtv")
    vim     = ((mod1Mask .|. shiftMask, xK_v), runInTerm "-title vim" "nvim")
    ranger  = ((mod1Mask .|. shiftMask, xK_t), runInTerm "-title ranger" "env EDITOR=nvim ranger")
    weechat = ((mod1Mask .|. shiftMask, xK_i), runInTerm "-title weechat" "weechat")
    mail    = ((mod1Mask .|. shiftMask, xK_m), runInTerm "-title mail" "sup-mail")
    htop    = ((mod1Mask .|. shiftMask, xK_h), runInTerm "-title htop" "htop")
    toggleFullScreen = ((mod1Mask, xK_f), sendMessage $ Toggle FULL)
