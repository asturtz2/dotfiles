{-# RecordWildCards NamedFieldPuns #-}
import System.IO

import Prelude hiding (mod)
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import Control.Monad

import XMonad hiding (config)
import qualified XMonad.StackSet as W

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.EqualSpacing
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
-- import XMonad.Layout.ResizableTile

import XMonad.Util.Run(spawnPipe, runInTerm)
import XMonad.Util.EZConfig(additionalKeys, removeKeys)
import XMonad.Util.Themes

import XMonad.Actions.WindowGo
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap
import XMonad.Actions.DynamicWorkspaces

main = xmonad $ fullscreenSupport config

-- config = extensions . hooks . settings $ def

-- data Hook = Startup | Layout | Manage | Event
config                      = extensions def
    { terminal              = term
    , borderWidth           = 0
    , normalBorderColor     = "#cccccc"
    , focusedBorderColor    = "#8A745E"
    , startupHook           = start
    , layoutHook            = layout
    , manageHook            = manager
    , handleEventHook       = events
    , modMask               = mod4Mask
    }

extensions :: XConfig l -> XConfig l
extensions = ewmh . docks . keybinds

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
    --compton
    spawn "xset r rate 250 40"
  where
    polybar = reload "polybar --reload main"
    --compton = reload "compton"

reload :: String -> X ()
reload process = spawn $ "reload " ++ process

term = "urxvt"
-- Layouts
-- layout :: l Window


-- layout = extend layouts
layout = id
    -- . simpleDeco shrinkText windowTheme
    . equalSpacing gapWidth gapShrink mult minWidth
    . avoidStruts
    . mkToggle (single FULL)
    . windowNavigation
    $ tiled ||| Mirror tiled
    -- ||| simpleTabbed ||| simplestFloat
  where
    -- compose = foldr (.) id
    -- layoutExtensions = concat [gaps, resizing, [windowNavigation]]
    -- gaps = [spacing 2, avoidStuts]
    -- resizing = [mkToggle (single FULL), minimize]
    -- extend = compose layoutExtensions
    gapWidth  = 15
    gapShrink = 5
    mult      = 0
    minWidth  = 1
    tiled     = Tall nmaster delta ratio
    nmaster   = 1
    delta     = 3/100
    ratio     = 1/2

windowTheme :: Theme
windowTheme = def
    { decoWidth = 2130
    , decoHeight = 40
    , fontName   = "Iosevka"
    , activeBorderColor = activeColor def
    }



manager = composeAll
    [ manageSpawn
    -- , ifLaunched office --> doFloat
    , manageHook defaultConfig
    -- , className =? "Dolphin-emu" --> doFloat
    , isDialog --> doF W.shiftMaster <+> doF W.swapDown
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

running a = patternIn appName <||> patternIn className <||> patternIn title
  where
    patternIn = fmap $ L.isInfixOf a

appLaunched :: String -> Query Bool
appLaunched app = L.isInfixOf app <$> title

events = fullscreenEventHook <+> docksEventHook <+> handleEventHook def

-- spaces :: [String]
-- spaces = [ "dev" , "web" , "mail" , "media" , "chat" , "office" , "log" ]

mod   = mod4Mask
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
    remove window    = L.delete window workspaceWindows

-- Kill all windows
killAll :: WindowSet -> X()
killAll windowSet = killWindows $ W.allWindows windowSet

-- Keymappings
keybinds :: XConfig l -> XConfig l
keybinds = flip additionalKeys addKeys . flip removeKeys delKeys
  where
    addKeys = concat
      [ systemKeys
      , movementKeys
      , fullscreen
      , insert
      , switch
      , monitor
      , delete
      , quit
      ]
    delKeys =
      [ (mod, xK_q)
      , (mod .|. shiftMask, xK_Return)
      , (mod .|. shiftMask, xK_c)
      , (mod, xK_p)
      ]


fullscreen :: [((KeyMask, KeySym), X())]
fullscreen = [bind xK_f (sendMessage $ Toggle FULL)]

bind :: KeySym -> X() -> ((KeyMask, KeySym), X ())
bind key action = ((mod, key), action)

bindNoMod :: Integral n => KeySym -> X() -> ((n, KeySym), X())
bindNoMod key action = ((0, key), action)

bindShift :: KeySym -> X() -> ((KeyMask, KeySym), X ())
bindShift key action = ((mod .|. shift, key), action)

-- Insert a new instance of an application into the current workspace before the
-- currently focused window
insert :: [((KeyMask, KeySym), X ())]
insert = [bind xK_i subkeys]
  where
    subkeys = submap . M.fromList $
      [ bindNoMod xK_e $ run   "vim"
      , bindNoMod xK_p $ runInTerm "-title ncmpcpp" "zsh -c 'ncmpcpp'"
      , bindNoMod xK_c $ run   "weechat"
      , bindNoMod xK_v $ spawn "zathura"
      , bindNoMod xK_b $ spawn "vimb"
      , bindNoMod xK_r $ spawn "rofi -show run"
      , bindNoMod xK_s $ spawn term
      ]

-- Switch to the next existing instance of an application in any workspace
switch :: [((KeyMask, KeySym), X ())]
switch = [bind xK_s subkeys]
  where
    subkeys = submap . M.fromList $
      [ bindNoMod xK_e $ raiseNext (running "vim")
      , bindNoMod xK_p $ raiseNext (running "ncmpcpp")
      , bindNoMod xK_v $ raiseNext (running "zathura")
      , bindNoMod xK_b $ raiseNext (running "vimb")
      , bindNoMod xK_c $ raiseNext (running "WeeChat")
      , bindNoMod xK_s $ raiseNext (running "surface")
      ]

monitor :: [((KeyMask, KeySym), X ())]
monitor = [bind xK_m subkeys]
  where
    subkeys = submap . M.fromList $
      [ bindNoMod xK_e $ spawn "mons -e top && xmonad --restart && wal -R"
      , bindNoMod xK_o $ spawn "mons -o top && xmonad --restart && wal -R"
      , bindNoMod xK_s $ spawn "mons -s top && xmonad --restart && wal -R"
      ]

delete :: [((KeyMask, KeySym), X ())]
delete = [bind xK_d subkeys]
  where
    subkeys = submap . M.fromList $
      [ bindNoMod xK_d kill
      , bindNoMod xK_w $ withWindowSet (killWindows . W.index)
      , bindNoMod xK_o $ withWindowSet killUnfocused
      , bindNoMod xK_a $ withWindowSet killAll
      ]

quit :: [((KeyMask, KeySym), X ())]
quit = [bind xK_q subkeys]
  where
    subkeys = submap . M.fromList $
      [ bindNoMod xK_q $ spawn "xmonad --recompile && xmonad --restart"
      , bindNoMod xK_p $ spawn "poweroff"
      , bindNoMod xK_r $ spawn "reboot"
      ]

-- open :: [((KeyMask, KeySym), X ())]
-- open = [bind xK_o openMappings]
--   where
--     openMappings  = submap . M.fromList $

movementKeys :: [((KeyMask, KeySym), X ())]
movementKeys = [moveDown, moveUp, moveRight, moveLeft]
  where
    moveUp    = bindShift xK_Up    (sendMessage $ Go U)
    moveDown  = bindShift xK_Down  (sendMessage $ Go D)
    moveLeft  = bindShift xK_Left  (sendMessage $ Go L)
    moveRight = bindShift xK_Right (sendMessage $ Go R)

-- windowKeys :: [((KeyMask, KeySym), X ())]
-- windowKeys = [fullscreen, onlyFocused, killAllWindows]
--   where
--     fullscreen     = bind      xK_f (sendMessage $ Toggle FULL)
--     onlyFocused    = bindShift xK_o (withWindowSet killUnfocused)
--     killAllWindows = bindShift xK_k (withWindowSet killAll)


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
run = runInTerm ""

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
