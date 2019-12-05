{-# LANGUAGE RebindableSyntax, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, RecordWildCards, NamedFieldPuns #-}
import System.IO

import qualified Prelude as P
import qualified Data.Char as Char
import qualified Data.List as L
import Data.Function (on)
import qualified Data.Map as M
-- import Control.Monad

import XMonad.Config.Prime
import qualified XMonad.StackSet as W

import XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GroupNavigation
import XMonad.Actions.Navigation2D
import XMonad.Actions.Launcher
import XMonad.Actions.TagWindows

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.UrgencyHook

-- import XMonad.Layout.Dishes
-- import XMonad.Layout.EqualSpacing
import XMonad.Layout.Accordion
import XMonad.Layout.Decoration hiding (modifyLayout)
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation

import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Pass
import XMonad.Prompt.Window
-- import XMonad.Layout.Minimize
-- import XMonad.Layout.PerWorkspace
-- import XMonad.Layout.SimpleDecoration
-- import qualified XMonad.Layout.SimpleFloat as Layout (simpleFloat)
-- import XMonad.Layout.Spacing
-- import XMonad.Layout.TwoPane
-- import XMonad.Layout.WindowNavigation

import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (runInTerm, safeSpawn)
-- import XMonad.Util.EZConfig(additionalKeys, additionalKeysP, additionalMouseBindings, removeKeys)
-- import XMonad.Util.Themes

-- import XMonad.Actions.FlexibleResize as Flex
-- import XMonad.Actions.WindowGo
-- import XMonad.Actions.SpawnOn
-- import XMonad.Actions.Submap

import Colors

-- Create fifo pipes for polybar
wsFifo, titleFifo :: String
wsFifo = "/tmp/xmonad-ws"
titleFifo = "/tmp/xmonad-title"

mkPipes :: IO ()
mkPipes = mapM_ mkfifo [ wsFifo, titleFifo ]
  where
    mkfifo file = safeSpawn "mkfifo" [ file ]

appendTitle :: X ()
appendTitle = do
    winset <- gets windowset
    title <- maybe (return "") (fmap show . getName) . W.peek $ winset
    io $ appendFile titleFifo $ format title ++ "\n"
  where
    (>>) :: X a -> X b -> X b
    (>>) = (P.>>)
    format title
      | L.length title > 50 = P.take 50 title ++ "..."
      | otherwise = title

appendWorkspaces :: X ()
appendWorkspaces = do
    winset <- gets windowset
    let
      currWs = W.currentTag winset
      wss = map W.tag . namedScratchpadFilterOutWorkspace $ W.workspaces winset
      wsStr = L.concat $ P.map (fmt currWs) $ sort' wss

    io $ appendFile wsFifo (wsStr ++ "\n")
  where
    (>>) :: X a -> X b -> X b
    (>>) = (P.>>)
    sort' = L.sortBy (compare `on` (!! 0))
    fmt currWs ws
      | currWs == ws = " %{F" ++ color3 ++ "}%{u" ++ color2 ++ "}%{+u}" ++ ws ++ "%{-u}%{F-} "
      | otherwise = " " ++ ws ++ " "

main :: IO ()
main = mkPipes P.>> (xmonad $ do

  ------------------------------------------------------------------------------
  -- Misc                                                                     --
  ------------------------------------------------------------------------------
  -- Basic window management support
  apply $ fullscreenSupport . ewmh . docks

  -- Navigation2D
  apply $ withNavigation2DConfig $ def
    { defaultTiledNavigation = centerNavigation
    , floatNavigation = centerNavigation
    , screenNavigation = lineNavigation
    }

  apply $ withUrgencyHook dzenUrgencyHook

  let
    term = "urxvt"
    browser = "vimb"
  terminal =: term
  modMask =: mod4Mask

  ------------------------------------------------------------------------------
  -- Theme                                                                    --
  ------------------------------------------------------------------------------
  let
    -- Sizes
    gap = 10
    topbar = 10
    border = 0
    prompt = 20
    status = 20

    -- Colors
    red = "#dc322f"
    yellow = "#b58900"
    orange = "#cb4b16"
    magenta = "#d33682"
    violet = "#6c71c4"
    blue = "#268bd2"
    cyan = "#2aa198"
    green = "#859900"
    active = color3
    inactive = color6

    -- Themes
    font = "xft:Iosevka:size=12"

    topBarTheme = def
      { fontName = font
      , inactiveBorderColor = color6
      , inactiveColor = color6
      , inactiveTextColor = color6
      , activeBorderColor = active
      , activeColor = active
      , activeTextColor = active
      , urgentBorderColor = red
      , urgentTextColor = yellow
      , decoHeight = topbar
      }

    tabTheme = def
      { fontName = font
      , activeColor = active
      , inactiveColor = inactive
      , activeBorderColor = active
      , inactiveBorderColor = inactive
      , activeTextColor = background
      , inactiveTextColor = background
      }

    promptTheme = def
      { font = font
      , bgColor = background
      , fgColor = foreground
      , fgHLight = foreground
      , bgHLight = color2
      , borderColor = background
      , promptBorderWidth = 0
      , alwaysHighlight = True
      , height = prompt
      , position = Top
      , changeModeKey = xK_semicolon
      , searchPredicate = fuzzyMatch
      , sorter = fuzzySort
      }

  borderWidth =: 0

  ------------------------------------------------------------------------------
  -- Workspaces/Projects                                                      --
  ------------------------------------------------------------------------------
  -- Workspaces
  let
    wsDash = "dash"
    wsWork = "work"
    wsMail = "mail"
    wsBoard = "board"
    wsVm = "vm"
    wsDb = "db"
    wsXm = "xmonad"
    wsVimb = "vimb"
    wsVimrc = "vimrc"
    wsSys = "sys"
    wsConductix = "conductix"

  workspaces =: [ wsDash, wsWork, wsMail, wsBoard ]

  -- Scratchpads
  let
    contains :: Query String -> String -> Query Bool
    query `contains` str = fmap predicate query
      where
        predicate q = str `L.isPrefixOf` q || str `L.isInfixOf` q || str `L.isSuffixOf` q

    scratchpads =
      [ NS "slack" "slack" (resource =? "slack") defaultFloating
      , NS "teams" "firefox --new-window teams.microsoft.com" (title `contains` "Teams") defaultFloating
      , NS "spotify" "spotify" (resource =? "spotify") defaultFloating
      -- This is a bit of a hack to make the emulator and sidebar move as one
      , NS "emulator" "emulator -avd Pixel_3_XL_API_27"
          (title `contains` "Android Emulator") defaultFloating
      , NS "emulator-bar" "emulator -avd Pixel_3_XL_API_27"
          (title =? "Emulator") defaultFloating
      ]

  -- Projects
  apply $ dynamicProjects
    [ Project
        { projectName = wsDash
        , projectDirectory = "~/"
        , projectStartHook = Nothing
        }
    , Project
        { projectName = wsWork
        , projectDirectory = "~/parse"
        , projectStartHook = Just $ spawn term
        }
    , Project
        { projectName = wsMail
        , projectDirectory = "~/"
        , projectStartHook = Just $ spawn "vimb"
        }
    , Project
        { projectName = wsBoard
        , projectDirectory = "~/"
        , projectStartHook = Just $ spawn "vimb"
        }
    , Project
        { projectName = wsConductix
        , projectDirectory = "~/parse/conductix/inetpub/wwwroot/qq/qq5-reel/"
        , projectStartHook = Just $
            runInTerm "-S ~/.vim/sessions/conductix" "vim -S ~/.vim/sessions/conductix"
              P.>> runInTerm "" "npm run dev"
              P.>> spawn "google-chrome-stable localhost:8080"
        }
    , Project
        { projectName = wsVm
        , projectDirectory = "~/"
        , projectStartHook = Just $ spawn "vmware"
        }
    , Project
        { projectName = wsDb
        , projectDirectory = "~/"
        , projectStartHook = Just $ spawn "azuredatastudio"
        }
    , Project
        { projectName = wsXm
        , projectDirectory = "~/dotfiles/xmonad/.xmonad"
        , projectStartHook = Just $
            runInTerm "-name vim" "vim xmonad.hs"
              P.>> spawn (browser ++ " https://hoogle.haskell.org")
        }
    , Project
        { projectName = wsVimb
        , projectDirectory = ".config/vimb"
        , projectStartHook = Just $ runInTerm "-name vim" "vim config"
        }
    , Project
        { projectName = wsVimrc
        , projectDirectory = "~/.vim"
        , projectStartHook = Just $ runInTerm "-name vim" "vim ~/.vimrc"
        }
    , Project
        { projectName = wsSys
        , projectDirectory = "~/"
        , projectStartHook = Just
            $ spawn term
            P.>> spawn (browser ++ " https://wiki.archlinux.org")
        }
    ]

  ------------------------------------------------------------------------------
  -- Startup                                                                  --
  ------------------------------------------------------------------------------

  startupHook =+ spawn "reload polybar --reload main"
  startupHook =+ spawn "reload compton"

  ------------------------------------------------------------------------------
  -- LogHook                                                                  --
  ------------------------------------------------------------------------------
  logHook =+ historyHook
  logHook =+ appendTitle
  logHook =+ appendWorkspaces

  ------------------------------------------------------------------------------
  -- ManageHooks                                                              --
  ------------------------------------------------------------------------------

  manageHook =+ namedScratchpadManageHook scratchpads
  manageHook =+ (isDialog --> doFloat)
  manageHook =+ (className =? "StardewValley.bin.x86_64" --> doFullFloat)
  manageHook =+ (className =? "jetbrains-studio" --> doFloat)
  manageHook =+ transience'

  ------------------------------------------------------------------------------
  -- Layouts                                                                  --
  ------------------------------------------------------------------------------

  let
    named n = renamed [ Replace n ]
    trimNamed w n = renamed [ CutWordsLeft w, PrependWords n ]
    suffixed n = renamed [ AppendWords n ]

    myGaps = gaps [(U, gap), (D, gap), (L, gap), (R, gap)]
    mySpacing = spacing gap

    addTopBar = noFrillsDeco shrinkText topBarTheme

    fullScreenToggle = mkToggle $ single FULL

    tabs = named "tabs"
      $ addTopBar
      $ addTabs shrinkText tabTheme
      $ Simplest

    flex = trimNamed 5 "Flex"
      $ avoidStruts
      $ windowNavigation
      $ addTopBar
      $ addTabs shrinkText tabTheme
      $ subLayout [] (Simplest ||| Accordion)
      $ myGaps
      $ mySpacing
      $ (suffixed "Std 7/12" $ ResizableTall 1 (1/20) (7/12) [])
        ||| (suffixed "Std 1/2" $ ResizableTall 1 (1/20) (1/2) [])


  resetLayout flex
  modifyLayout fullScreenToggle

  ------------------------------------------------------------------------------
  -- Keybinds                                                                 --
  ------------------------------------------------------------------------------

  let
    dirKeys = [ "h", "j", "k", "l" ]
    dirs = [ L, D, U, R ]
    zipKeys prefix action = P.zipWith (\k d -> (prefix ++ k, action d)) dirKeys dirs

  -- Mouse bindings
  mouseBindings =+ [((mod4Mask .|. shiftMask, button1), (\w -> focus w P.>> Flex.mouseResizeWindow w))]
  -- System keys
  keys =+
    [ ("M-x", spawn "betterlockscreen -l dimblur")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -u" P.>> spawn "pamixer -i 5")
    , ("<XF86AudioLowerVolume>", spawn "pamixer -u" P.>> spawn "pamixer -d 5")
    , ("<XF86AudioMute>", spawn "pamixer -m")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioPrev>", spawn "playerctl prev")
    ]

  -- Sublayouts
  keys =+
    zipKeys "M-s " (sendMessage . pullGroup)
    ++
    zipKeys "M-s M-" (sendMessage . pullGroup)
    ++
    [ ("M-s u", withFocused $ sendMessage . UnMerge)
    , ("M-s M-u", withFocused $ sendMessage . UnMerge)
    , ("M-s <Tab>", toSubl NextLayout)
    , ("M-;", onGroup W.focusDown')
    , ("M-S-;", onGroup W.focusUp')
    ]

  -- Toggles

  let
    toggleFloat w = windows (\s ->
      if M.member w $ W.floating s
      then W.sink w s
      else (W.float w (W.RationalRect (1/8) (1/6) (2/3) (3/5)) s))

  keys =+
    [ ("M-f", sendMessage $ Toggle FULL)
    , ("M-<Tab>", sendMessage $ NextLayout)
    , ("M-y", withFocused toggleFloat)
    ]

  -- Prompts
  let
    killWorkspace =
      (withWindowSet $ P.mconcat . map killWindow . W.index) P.>> removeWorkspace

  keys =+
    [ ("M-p p", switchProjectPrompt promptTheme)
    , ("M-p M-p", switchProjectPrompt promptTheme)
    , ("M-p d", changeProjectDirPrompt promptTheme)
    , ("M-p M-d", changeProjectDirPrompt promptTheme)
    , ("M-p s", passTypePrompt promptTheme)
    , ("M-p M-s", passTypePrompt promptTheme)
    , ("M-p k", killWorkspace)
    , ("M-p M-k", killWorkspace)
    ]
  -- Applications
  let
    toggleEmulator =
      namedScratchpadAction scratchpads "emulator"
        P.>> namedScratchpadAction scratchpads "emulator-bar"

  keys =+
    [ ("M-r r", spawn "rofi -show run")
    , ("M-r M-r", spawn "rofi -show run")
    , ("M-r b", spawn browser)
    , ("M-r M-b", spawn browser)
    , ("M-r d", runInTerm "" "ddgr")
    , ("M-r M-d", runInTerm "" "ddgr")
    , ("M-r g", runInTerm "" "googler")
    , ("M-r M-g", runInTerm "" "googler")
    , ("M-z c", namedScratchpadAction scratchpads "slack")
    , ("M-z M-c", namedScratchpadAction scratchpads "slack")
    , ("M-z t", namedScratchpadAction scratchpads "teams")
    , ("M-z M-t", namedScratchpadAction scratchpads "teams")
    , ("M-z p", namedScratchpadAction scratchpads "spotify")
    , ("M-z M-p", namedScratchpadAction scratchpads "spotify")
    , ("M-z e", toggleEmulator)
    , ("M-z M-e", toggleEmulator)
    , ("M-t", launcherPrompt promptTheme $ defaultLauncherModes $ LauncherConfig { browser = "firefox", pathToHoogle = "hoogle" })
    ]

  -- History
  let
    markJumped = withFocused $ addTag "jumped"
    unmarkJumped = withFocused $ delTag "jumped"
    jumped = ask P.>>= (\win -> liftX $ hasTag "jumped" win)
    notJumped = ask P.>>= (\win -> liftX $ not <$> hasTag "jumped" win)

  keys =+
    [ ("M-o", markJumped P.>> nextMatch History notJumped)
    , ("M-i", unmarkJumped P.>> nextMatch History jumped)
    , ("M-b e", unmarkJumped P.>> (nextMatch History $ title `contains` "VIM"))
    , ("M-b M-e", unmarkJumped P.>> (nextMatch History $ title `contains` "VIM"))
    , ("M-b b", unmarkJumped P.>> (nextMatch History $ className =? "Vimb"))
    , ("M-b M-b", unmarkJumped P.>> (nextMatch History $ className =? "Vimb"))
    ]

  -- Window navigation
  let
    searchWindows = windowMultiPrompt promptTheme [(Goto, allWindows), (Goto, wsWindows)]
  keys =+
    zipKeys "M-" (flip windowGo True)
    ++
    zipKeys "M-S-" (flip windowSwap True)
    ++
    [ ("M-/", unmarkJumped P.>> searchWindows)
    , ("M-n", nextMatchWithThis Forward className)
    , ("M-S-n", nextMatchWithThis Backward className)
    , ("M-<Backspace>", nextMatch History $ return True)
    , ("M-[", sendMessage Shrink)
    , ("M-]", sendMessage Expand)
    , ("M-S-[", sendMessage MirrorShrink)
    , ("M-S-]", sendMessage MirrorExpand)
    ]
  -- Marks
  let
    addMark :: Char -> X ()
    addMark char = delMark P.>> (withFocused $ addTag $ [char])
      where
        delMark = withTaggedGlobal [char] $ delTag [char]

    jumpMark :: Char -> X ()
    jumpMark char = focusUpTaggedGlobal [char]

  keys =+ P.map (\k -> ("M-m " ++ [k], addMark k)) ['a'..'z']
    ++ P.map (\k -> ("M-m M-" ++ [k], addMark k)) ['a'..'z']
    ++ P.map (\k -> ("M-' " ++ [k], jumpMark k)) ['a'..'z']
    ++ P.map (\k -> ("M-' M-" ++ [k], jumpMark k)) ['a'..'z'])
-- config = extensions def
--     { terminal              = term
--     , borderWidth           = 2
--     , normalBorderColor     = background
--     , focusedBorderColor    = color3
--     , startupHook           = start
--     , layoutHook            = layout
--     , manageHook            = manager
--     , logHook               = historyHook
--     , handleEventHook       = events
--     , modMask               = mod4Mask
--     }

-- extensions :: XConfig l -> XConfig l
-- extensions = ewmh . docks . keybinds . mouseBinds

-- -- hooks :: XConfig l -> XConfig l
-- -- hooks = startup . manage . layout . events

-- -- settings :: XConfig l -> XConfig l
-- -- settings = term . width .

-- -- -- startupHook def = spawn "ls"


-- startup :: XConfig l -> XConfig l
-- startup conf = conf {startupHook = start}

-- start :: X()
-- start = do
--     polybar
--     compton
--     spawn "xset r rate 250 40"
--   where
--     polybar = reload "polybar --reload main"
--     compton = reload "compton"

-- reload :: String -> X ()
-- reload process = spawn $ "reload " ++ process

-- term = "urxvt"
-- -- Layouts
-- -- layout :: l Window


-- data FLOAT = FLOAT deriving (Read, Show, Eq, Typeable)
-- instance Transformer FLOAT Window where
--     transform _ layout f = f Layout.simpleFloat (\_ -> layout)

-- layout = id
--     -- . simpleDeco shrinkText windowTheme
--     . equalSpacing gapWidth gapShrink mult minWidth
--     . avoidStruts
--     . mkToggle (single FULL)
--     . mkToggle (single FLOAT)
--     . windowNavigation
--     $ tiled ||| Mirror tiled ||| simpleTabbedBottom
--     -- ||| simpleTabbed ||| simpleFloat
--   where
--     -- compose = foldr (.) id
--     -- layoutExtensions = concat [gaps, resizing, [windowNavigation]]
--     -- gaps = [spacing 2, avoidStuts]
--     -- resizing = [mkToggle (single FULL), minimize]
--     -- extend = compose layoutExtensions
--     gapWidth  = 15
--     gapShrink = 5
--     mult      = 0
--     minWidth  = 1
--     tiled     = Tall nmaster delta ratio
--     nmaster   = 1
--     delta     = 3/100
--     ratio     = 1/2

-- windowTheme :: Theme
-- windowTheme = def
--     { decoWidth = 2130
--     , decoHeight = 40
--     , fontName   = "Iosevka"
--     , activeBorderColor = activeColor def
--     }



-- manager = composeAll
--     [ placeHook $ withGaps (16,0,16,0) (smart (0.5,0.5))
--     , manageSpawn
--     , manageHook def
--     , isDialog --> doF W.shiftMaster <+> doF W.swapDown
--     ]
--   where
--     -- viewShift = doF . liftM2 (.) W.view W.shift
--     browsers = ["qutebrowser", "google-chrome", "firefox"]
--     media    = ["spotify", "mpv"]
--     office   = "libreoffice-calc"

-- running a = patternIn appName <||> patternIn className <||> patternIn title
--   where
--     patternIn = fmap $ L.isInfixOf a

-- appLaunched :: String -> Query Bool
-- appLaunched app = L.isInfixOf app <$> title

-- events = fullscreenEventHook <+> docksEventHook <+> handleEventHook def

-- -- spaces :: [String]
-- -- spaces = [ "dev" , "web" , "mail" , "media" , "chat" , "office" , "log" ]

-- mod   = mod4Mask
-- shift = shiftMask

-- -- killRest :: Window -> X()
-- -- killRest window = mconcat (map killWindow (W.index windowset))

-- -- Kill the given list of windows
-- killWindows :: [Window] -> X()
-- killWindows windows = mconcat $ map killWindow windows

-- -- Kill all unfocused windows within the current workspace
-- killUnfocused :: WindowSet -> X()
-- killUnfocused windowSet = killWindows unfocusedWindows
--   where
--     workspaceWindows = W.index windowSet
--     focusedWindow    = W.peek windowSet
--     unfocusedWindows = maybe [] remove focusedWindow
--     remove window    = L.delete window workspaceWindows

-- -- Kill all windows
-- killAll :: WindowSet -> X()
-- killAll windowSet = killWindows $ W.allWindows windowSet

-- -- Keymappings
-- keybinds :: XConfig l -> XConfig l
-- keybinds = soundKeys . flip additionalKeys addKeys . flip removeKeys delKeys
--   where
--     addKeys = concat
--       [ systemKeys
--       , movementKeys
--       , toggles
--       , insert
--       , switch
--       , navigation
--       , rofi
--       , monitor
--       , delete
--       , quit
--       ]
--     delKeys =
--       [ (mod, xK_q)
--       , (mod .|. shiftMask, xK_Return)
--       , (mod .|. shiftMask, xK_c)
--       , (mod, xK_p)
--       ]

-- rofi :: [((KeyMask, KeySym), X())]
-- rofi = [ ((mod, xK_g), spawn "rofi -show window")]

-- mouseBinds :: XConfig l -> XConfig l
-- mouseBinds = flip additionalMouseBindings addBinds
--   where
--     addBinds = [ ((mod, button1), (\w -> focus w >> Flex.mouseResizeWindow w)) ]

-- toggles :: [((KeyMask, KeySym), X())]
-- toggles = [bind xK_f subkeys]
--   where
--     subkeys = submap . M.fromList $
--       [ bindNoMod xK_f $ (sendMessage $ Toggle FULL)
--       , bindNoMod xK_u $ (sendMessage $ Toggle FLOAT)
--       ]

-- soundKeys :: XConfig l -> XConfig l
-- soundKeys c = additionalKeysP c
--     [ ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5")
--     , ("<XF86AudioLowerVolume>", spawn "pamixer -d 5")
--     , ("<XF86AudioMute>", spawn "pamixer -t")
--     ]
-- bind :: KeySym -> X() -> ((KeyMask, KeySym), X ())
-- bind key action = ((mod, key), action)

-- bindNoMod :: Integral n => KeySym -> X() -> ((n, KeySym), X())
-- bindNoMod key action = ((0, key), action)

-- bindShift :: KeySym -> X() -> ((KeyMask, KeySym), X ())
-- bindShift key action = ((mod .|. shift, key), action)

-- -- Insert a new instance of an application into the current workspace before the
-- -- currently focused window
-- insert :: [((KeyMask, KeySym), X ())]
-- insert = [bind xK_i subkeys]
--   where
--     subkeys = submap . M.fromList $
--       [ bindNoMod xK_e $ run   "vim"
--       , bindNoMod xK_p $ runInTerm "-title ncmpcpp" "zsh -c 'ncmpcpp'"
--       , bindNoMod xK_c $ run   "weechat"
--       , bindNoMod xK_v $ spawn "zathura"
--       , bindNoMod xK_b $ spawn "vimb"
--       , bindNoMod xK_r $ spawn "rofi -show run"
--       , bindNoMod xK_s $ spawn term
--       ]

-- -- Switch to the next existing instance of an application in any workspace
-- switch :: [((KeyMask, KeySym), X ())]
-- switch = [bind xK_s subkeys]
--   where
--     subkeys = submap . M.fromList $
--       [ bindNoMod xK_e $ nextMatch History (running "VIM")
--       , bindNoMod xK_p $ nextMatch History (running "ncmpcpp")
--       , bindNoMod xK_v $ nextMatch History (running "zathura")
--       , bindNoMod xK_b $ nextMatch History (running "Firefox")
--       , bindNoMod xK_c $ nextMatch History (running "WeeChat")
--       , bindNoMod xK_s $ nextMatch History (running "surface")
--       ]

-- navigation :: [((KeyMask, KeySym), X ())]
-- navigation =
--     [ ((mod, xK_n), nextMatchWithThis Forward appName)
--     , ((mod .|. shift, xK_n), nextMatchWithThis Backward appName)
--     , ((mod, xK_Return), nextMatch History $ return True)
--     ]

-- monitor :: [((KeyMask, KeySym), X ())]
-- monitor = [bind xK_m subkeys]
--   where
--     subkeys = submap . M.fromList $
--       [ bindNoMod xK_e $ spawn "mons -e top && xmonad --restart && wal -R"
--       , bindNoMod xK_o $ spawn "mons -o top && xmonad --restart && wal -R"
--       , bindNoMod xK_s $ spawn "mons -s top && xmonad --restart && wal -R"
--       ]

-- delete :: [((KeyMask, KeySym), X ())]
-- delete = [bind xK_d subkeys]
--   where
--     subkeys = submap . M.fromList $
--       [ bindNoMod xK_d kill
--       , bindNoMod xK_w $ withWindowSet (killWindows . W.index)
--       , bindNoMod xK_o $ withWindowSet killUnfocused
--       , bindNoMod xK_a $ withWindowSet killAll
--       ]

-- quit :: [((KeyMask, KeySym), X ())]
-- quit = [bind xK_q subkeys]
--   where
--     subkeys = submap . M.fromList $
--       [ bindNoMod xK_q $ spawn "xmonad --recompile && xmonad --restart"
--       , bindNoMod xK_p $ spawn "poweroff"
--       , bindNoMod xK_r $ spawn "reboot"
--       ]

-- -- open :: [((KeyMask, KeySym), X ())]
-- -- open = [bind xK_o openMappings]
-- --   where
-- --     openMappings  = submap . M.fromList $

-- movementKeys :: [((KeyMask, KeySym), X ())]
-- movementKeys = [moveDown, moveUp, moveRight, moveLeft]
--   where
--     moveUp    = bindShift xK_Up    (sendMessage $ Go U)
--     moveDown  = bindShift xK_Down  (sendMessage $ Go D)
--     moveLeft  = bindShift xK_Left  (sendMessage $ Go L)
--     moveRight = bindShift xK_Right (sendMessage $ Go R)

-- -- windowKeys :: [((KeyMask, KeySym), X ())]
-- -- windowKeys = [fullscreen, onlyFocused, killAllWindows]
-- --   where
-- --     fullscreen     = bind      xK_f (sendMessage $ Toggle FULL)
-- --     onlyFocused    = bindShift xK_o (withWindowSet killUnfocused)
-- --     killAllWindows = bindShift xK_k (withWindowSet killAll)


-- -- workspaceKeys :: [((KeyMask, KeySym), X ())]
-- -- workspaceKeys = [openWorkspace]
-- --   where
-- --     openWorkspace = bind xK_o (addWorkspace

-- systemKeys :: [((KeyMask, KeySym), X ())]
-- systemKeys = [reboot, poweroff, wal]
--   where
--     reboot   = bindShift xK_r (spawn "reboot")
--     poweroff = bindShift xK_p (spawn "poweroff")
--     wal      = bind xK_a (spawn "wal -i ~/wallpapers")

-- run :: String -> X ()
-- run = runInTerm ""

--     -- launcher = ((mod , xK_v), raiseEditor)





--     -- reload       = ((mod, xK_q),           spawn "reload-xmonad")
--     -- ranger  =
--     --   ( (mod .|. shift, xK_t)
--     --   , runInTerm "-title ranger" "env EDITOR=nvim ranger"
--     --   )
--     -- weechat =
--     --   ( (mod .|. shift, xK_i)
--     --   , runInTerm "-title weechat" "weechat"
--     --   )
--     -- mail    =
--     --   ( (mod .|. shift, xK_m)
--     --   , runInTerm "-title mail" "sup-mail"
--     --   )
