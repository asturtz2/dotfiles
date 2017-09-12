import System.IO

import Prelude hiding (mod)
import Data.List
import Control.Monad

import XMonad hiding (config)
import qualified XMonad.StackSet as W

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.WindowNavigation
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
    { terminal           = term
    , borderWidth        = 4
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#8A745E"
    , layoutHook         = layout
    , manageHook         = manager
    , handleEventHook    = events
    , logHook            = logger
    , workspaces         = spaces
    } `additionalKeys` keybinds

term = "urxvt"
-- Layouts
-- layout :: l Window
layout = id
    . equalSpacing gapWidth gapShrink mult minWidth
    . avoidStruts
    . mkToggle (single FULL)
    . minimize
    . windowNavigation
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

manager = composeAll . concat $
    [ [ifLaunched a --> viewShift "web" | a <- browsers]
    , [ifLaunched a --> viewShift "media" | a <- media]
    , [ifLaunched "nvim" --> viewShift "dev"]
    , [ifLaunched "weechat" --> viewShift "chat"]
    , [manageSpawn]
    , [manageDocks]
    , [manageHook defaultConfig]
    ]
  where
    viewShift = doF . liftM2 (.) W.view W.shift
    browsers = ["qutebrowser", "google-chrome", "firefox"]
    media    = ["spotify", "mpv"]

ifLaunched a = appName =? a <||> className =? a <||> title =? a

logger = ewmhDesktopsLogHook

events = handleEventHook def <+> fullscreenEventHook

spaces :: [String]
spaces = [ "dev" , "web" , "mail" , "media" , "chat" , "office" , "log" ]

mod   = mod1Mask
shift = shiftMask

-- killRest :: Window -> X()
-- killRest window = mconcat (map killWindow (W.index windowset))

-- Kill the given list of windows
killWindows :: [Window] -> X()
killWindows windows = mconcat $ map killWindow windows

-- Kill all unfocused windows within the current workspace
killUnfocused :: WindowSet -> X()
killUnfocused windowSet = killWindows unfocusedWindows
  where
    workspaceWindows = W.index windowSet
    focusedWindow = W.peek windowSet
    unfocusedWindows = maybe [] remove focusedWindow
    remove window = delete window workspaceWindows

-- Kill all windows
killAll :: WindowSet -> X()
killAll windowSet = killWindows allWindows
  where
    allWindows = W.allWindows windowSet

-- Keymappings
keybinds :: [((KeyMask, KeySym), X ())]
keybinds = systemKeys ++ appKeys

systemKeys :: [((KeyMask, KeySym), X ())]
systemKeys =
    [ moveDown
    , moveUp
    , moveRight
    , moveLeft
    , fullscreen
    , onlyFocused
    , noWindows
    , poweroff
    , reboot
    ]
  where

    moveDown    = ((mod           , xK_Down)  , sendMessage $ Go D)
    moveUp      = ((mod           , xK_Up)    , sendMessage $ Go U)
    moveRight   = ((mod           , xK_Left)  , sendMessage $ Go R)
    moveLeft    = ((mod           , xK_Right) , sendMessage $ Go L)
    fullscreen  = ((mod           , xK_f)     , sendMessage $ Toggle FULL)
    onlyFocused = ((mod .|. shift , xK_o)     , withWindowSet killUnfocused)
    noWindows   = ((mod .|. shift , xK_k)     , withWindowSet killAll)
    poweroff    = ((mod .|. shift , xK_p)     , spawn "poweroff")
    reboot      = ((mod .|. shift , xK_r)     , spawn "reboot")


    -- play        = ((mod           , xK_XF86AudioPlay) , sendMessage $ Toggle FULL)
run :: String -> X ()
run command = runInTerm "" command

-- ifNo :: String -> Query Bool
-- ifNo command = checkTitle (<||>) checkAppName (<||>) checkClassName
--   where
--     checkTitle     = title =? command
--     checkAppName   = appName =? command
--     checkClassName = className =? command


appKeys :: [((KeyMask, KeySym), X ())]
appKeys =
    [ editor
    , launcher
    , explorer
    , browser
    , viewer
    , player
    , irc
    , nextTerm
    -- , mail
    ]
  where
    editor     = ((mod , xK_v), raiseMaybe (run "nvim") (ifLaunched "nvim"))
    launcher   = ((mod , xK_g), spawn "rofi -show run")
    explorer   = ((mod , xK_d), run "ranger")
    browser    = ((mod , xK_b), raiseBrowser)
    viewer     = ((mod , xK_z), raiseNextMaybe (spawn "zathura") (ifLaunched "zathura"))
    player     = ((mod , xK_s), raiseMaybe (runSpotify) (ifLaunched "spotify"))
    irc        = ((mod , xK_c), raiseMaybe (run "weechat") (ifLaunched "weechat"))
    runSpotify = spawn "spotify --force-device-scale-factor=1.5"
    nextTerm = ((mod , xK_t), raiseNext $ ifLaunched term)

    -- launcher = ((mod , xK_v), raiseEditor)





    -- reload       = ((mod, xK_q),           spawn "reload-xmonad")
    -- ranger  =
    --   ( (mod .|. shift, xK_t)
    --   , runInTerm "-title ranger" "env EDITOR=nvim ranger"
    --   )
    -- weechat =
    --   ( (mod .|. shift, xK_i)
    --   , runInTerm "-title weechat" "weechat"
    --   )
    -- mail    =
    --   ( (mod .|. shift, xK_m)
    --   , runInTerm "-title mail" "sup-mail"
    --   )
