import System.IO

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.EqualSpacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace

import XMonad.Util.Run(spawnPipe, runInTerm)
import XMonad.Util.EZConfig(additionalKeys)

import XMonad.Actions.WindowGo
import XMonad.Actions.SpawnOn

import XMonad.Prompt

main = do
    xmproc  <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ def
        { startupHook        = startup
        , logHook            = logHook' xmproc
        , manageHook         = manageHook'
        , workspaces         = workspaces'
        , layoutHook         = layout
        , borderWidth        = borderWidth'
        , terminal           = terminal'
        , normalBorderColor  = normalBorderColor'
        , focusedBorderColor = focusedBorderColor'
        } `additionalKeys`
        [ toggleFullScreen
        , firefox
        , zathura
        , rtv
        , vim
        , ranger
        , weechat
        , mail
        ]

-- Basic configs
borderWidth' = 1
terminal' = "urxvt"
normalBorderColor' = "#cccccc"
focusedBorderColor' = "#8A745E"

startup :: X ()
startup = runInTerm "" "!wal"

logHook' xmproc = dynamicLogWithPP xmobarPP
    { ppOutput = hPutStrLn xmproc
    , ppTitle  = xmobarColor "brown" "" . shorten 50
    }

manageHook' = manageDocks <+> manageWorkspaces <+> manageHook defaultConfig
manageWorkspaces = composeAll
    [ className =? "firefox"    --> doShift "3:web"
    ]
workspaces' = ["1:Main", "2:Dev", "3:Web", "4:Viewer", "5:Chat", "6:Mail", "7:Music", "8:Video", "9:Scratch"]
-- Layouts
layout = id
    . equalSpacing gapWidth gapShrink mult minWidth
    . avoidStruts
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

-- Keymappings
-- Should be of type XConfig a -> [((ButtonMask, KeySym), X ())] -> XConfig a 
-- keybind :: XConfig a -> [((ButtonMask, KeySym), X ())] -> XConfig a

firefox = ((mod1Mask .|. shiftMask, xK_b), spawn "firefox")
zathura = ((mod1Mask .|. shiftMask, xK_z), spawn "zathura")
rtv     = ((mod1Mask .|. shiftMask, xK_r), runInTerm "-title rtv" "rtv")
vim     = ((mod1Mask .|. shiftMask, xK_v), runInTerm "-title vim" "nvim")
ranger  = ((mod1Mask .|. shiftMask, xK_t), runInTerm "-title ranger" "env EDITOR=nvim ranger")
weechat = ((mod1Mask .|. shiftMask, xK_i), runInTerm "-title weechat" "weechat")
mail    = ((mod1Mask .|. shiftMask, xK_m), runInTerm "-title mail" "sup-mail")
toggleFullScreen = ((mod1Mask, xK_f), sendMessage $ Toggle FULL)
