{-# RecordWildCards NameFieldPuns #-}
import System.IO

import Prelude hiding (mod)
import Data.List
import qualified Data.Map.Lazy as M
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
import XMonad.Layout.SubLayouts
-- import XMonad.Layout.ResizableTile

import XMonad.Util.Run(spawnPipe, runInTerm)
import XMonad.Util.EZConfig(additionalKeys)

import XMonad.Actions.WindowGo
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap
import XMonad.Actions.DynamicWorkspaces

import XMonad.Prompt

main = xmonad config

-- config = extensions . hooks . settings $ def

-- data Hook = Startup | Layout | Manage | Event
config = extensions def
    { terminal           = term
    , borderWidth        = 4
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#8A745E"
    , startupHook        = start
    , layoutHook         = layout
    , manageHook         = manager
    , handleEventHook    = events
    -- , workspaces         = ["1"]
    }

extensions :: XConfig l -> XConfig l
extensions = docks . ewmh . keybinds

-- hooks :: XConfig l -> XConfig l
-- hooks = startup . manage . layout . events

-- settings :: XConfig l -> XConfig l
-- settings = term . width .

-- -- startupHook def = spawn "ls"


startup :: XConfig l -> XConfig l
startup conf = conf {startupHook = start}

start :: X()
start = do
    polybar
    compton
    spawn "xset r rate 250 40"
  where
    polybar = reload "polybar --reload main"
    compton = reload "compton"

reload :: String -> X ()
reload process = spawn $ "reload " ++ process

term = "urxvt"
-- Layouts
-- layout :: l Window


-- layout = extend layouts
layout = id
    . equalSpacing gapWidth gapShrink mult minWidth
    . avoidStruts
    . mkToggle (single FULL)
    . subTabbed
    . minimize
    . windowNavigation
    $ tiled ||| Mirror tiled ||| simpleTabbed ||| simplestFloat
  where
    -- compose = foldr (.) id
    -- layoutExtensions = concat [gaps, resizing, [windowNavigation]]
    -- gaps = [spacing 2, avoidStuts]
    -- resizing = [mkToggle (single FULL), minimize]
    -- extend = compose layoutExtensions
    gapWidth  = 30
    gapShrink = 5
    mult      = 0
    minWidth  = 1
    tiled     = Tall nmaster delta ratio
    nmaster   = 1
    delta     = 3/100
    ratio     = 1/2


manager = composeAll
    [ manageSpawn
    -- , ifLaunched office --> doFloat
    , manageHook defaultConfig
    -- [ifLaunched a --> viewShift "web" | a <- browsers]
    -- , [ifLaunched a --> viewShift "media" | a <- media]
    -- , [ifLaunched "nvim" --> viewShift "dev"]
    -- , [ifLaunched "weechat" --> viewShift "chat"]
    ]
  where
    -- viewShift = doF . liftM2 (.) W.view W.shift
    browsers = ["qutebrowser", "google-chrome", "firefox"]
    media    = ["spotify", "mpv"]
    office   = "libreoffice-calc"

ifLaunched a = appName =? a <||> className =? a <||> title =? a

events = fullscreenEventHook <+> docksEventHook <+> handleEventHook def

-- spaces :: [String]
-- spaces = [ "dev" , "web" , "mail" , "media" , "chat" , "office" , "log" ]

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
    focusedWindow    = W.peek windowSet
    unfocusedWindows = maybe [] remove focusedWindow
    remove window    = delete window workspaceWindows

-- Kill all windows
killAll :: WindowSet -> X()
killAll windowSet = killWindows $ W.allWindows windowSet

-- Keymappings
keybinds :: XConfig l -> XConfig l
keybinds = flip additionalKeys $
    concat [systemKeys, movementKeys, windowKeys, appKeys]

bind :: KeySym -> X() -> ((KeyMask, KeySym), X ())
bind key action = ((mod, key), action)
  where
    mod = mod1Mask

bindShift :: KeySym -> X() -> ((KeyMask, KeySym), X ())
bindShift key action = ((mod .|. shift, key), action)
  where
    mod   = mod1Mask
    shift = shiftMask

movementKeys :: [((KeyMask, KeySym), X ())]
movementKeys = [moveDown, moveUp, moveRight, moveLeft]
  where
    moveUp    = bindShift xK_Up    (sendMessage $ Go U)
    moveDown  = bindShift xK_Down  (sendMessage $ Go D)
    moveLeft  = bindShift xK_Left  (sendMessage $ Go L)
    moveRight = bindShift xK_Right (sendMessage $ Go R)

windowKeys :: [((KeyMask, KeySym), X ())]
windowKeys = [fullscreen, onlyFocused, killAllWindows]
  where
    fullscreen     = bind      xK_f (sendMessage $ Toggle FULL)
    onlyFocused    = bindShift xK_o (withWindowSet killUnfocused)
    killAllWindows = bindShift xK_k (withWindowSet killAll)


-- workspaceKeys :: [((KeyMask, KeySym), X ())]
-- workspaceKeys = [openWorkspace]
--   where
--     openWorkspace = bind xK_o (addWorkspace

systemKeys :: [((KeyMask, KeySym), X ())]
systemKeys = [reboot, poweroff, wal]
  where
    reboot   = bindShift xK_r (spawn "reboot")
    poweroff = bindShift xK_p (spawn "poweroff")
    wal      = bind xK_a (spawn "wal -i ~/wallpapers")

run :: String -> X ()
run command = runInTerm "" command

appKeys :: [((KeyMask, KeySym), X ())]
appKeys =
    [ editor
    , launcher
    , explorer
    , browser
    , viewer
    , player
    , irc
    -- , mail
    ]
  where
    editor     = ((mod , xK_v), raiseMaybe (run "nvim") (ifLaunched "nvim"))
    launcher   = ((mod , xK_g), spawn "rofi -show run")
    explorer   = ((mod , xK_d), run "ranger")
    browser    = ((mod , xK_b), raiseMaybe (spawn "qutebrowser --backend webengine") (ifLaunched "qutebrowser"))
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
