import XMonad

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.List (isPrefixOf, (\\), tails)
import Data.Maybe (isNothing, fromMaybe, isJust, catMaybes)

import System.IO
import System.Posix.Unistd
import System.Posix.Files

import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import Control.Monad (when, replicateM)
import Control.Monad.Trans (liftIO)

import System.Directory
import System.FilePath

import Text.Printf (printf)

-- Hooks -----------------------------------------------------

import XMonad.Hooks.DynamicLog hiding (pprWindowSet)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- Layout ----------------------------------------------------
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.HintedTile
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Named
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

-- Actions ---------------------------------------------------

import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS

import XMonad.Actions.Submap
import XMonad.Actions.WindowGo

import XMonad.Actions.WithAll
import XMonad.Actions.SpawnOn

import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.WorkspaceNames as WN

import XMonad.Actions.GridSelect
import XMonad.Actions.Search

-- Prompts ---------------------------------------------------

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace

-- Utilities -------------------------------------------------

import XMonad.Util.EZConfig         -- (29) "M-C-x" style keybindings
import XMonad.Util.Run              -- (31) for 'spawnPipe', 'hPutStrLn'
import XMonad.Util.NamedScratchpad

import XMonad.Util.Cursor
--import XMonad.Util.XSelection
--import XMonad.Util.XUtils
-- Stuff to get Java working --------------------------------

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

main :: IO ()                                                     -- (31)
main = do
  status <- spawnPipe myStatusBar
  -- spawn "xcompmgr -c"
  -- spawn "inotify-daemon"
  host <- getHost
  xmonad $ myConfig status host                         -- (0)

myStatusBar :: String
myStatusBar = "xmobar $XMONAD/xmobarrc"

data Host = Desktop | Laptop Bool -- ^ Does the laptop have a Windows key?
  deriving (Eq, Read, Show)

getHost :: IO Host
getHost = do
  hostName <- nodeName `fmap` getSystemID
  return $ case hostName of
    "Oasis" -> Laptop True
    _       -> Desktop

myTerminal = "qterminal"
myShell = "fish"

myConfig status host =
     defaultConfig
       { borderWidth        = 2
       , terminal           = myTerminal
       , workspaces         = map show [1..9]
       , modMask            = if host == Laptop False
                                then modMask defaultConfig
                                else mod4Mask

       , normalBorderColor  = "#999999"
       , focusedBorderColor = "#009966"

       , logHook            = myLogHook status host
       , manageHook         = manageDocks
                              <+> manageSpawn
                              <+> myManageHook
                              <+> manageHook defaultConfig
       , layoutHook         = myLayoutHook
       , handleEventHook    = handleEventHook defaultConfig <+> docksEventHook
       , focusFollowsMouse  = True

       , startupHook        = do
                                ewmhDesktopsStartup
                                setWMName "LG3D"
                                checkKeymap (myConfig status host) (myKeys status host)
                                spawn "fcitx"
                                setDefaultCursor xC_left_ptr
                                -- spawn "[[ -f $ZSH_CUSTOM/../startup.zsh ]] && source $ZSH_CUSTOM/../startup.zsh"
                                spawn "feh --bg-scale $HOME/wallpaper/xmonad.png"
                                spawn "trayer --edge top --align right --widthtype percent --width 10 --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x000000 --expand true --heighttype pixel --height 25"
                                spawn "volti"
       , mouseBindings      = myMouseBindings
       }
       `additionalKeysP` (myKeys status host)                        -- (29)

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


spawnShellWith :: String -> X ()
spawnShellWith what = spawn (myTerminal ++ printf " -e '%s'" what)

spawnShell :: X ()
spawnShell = spawnShellWith myShell

spawnEditor :: X ()
spawnEditor = spawn ("code")

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

-- Statusbar
--
myLogHook status host = dynamicLogWithPP $ (myXmobarPP host) { ppOutput = hPutStrLn status }

myXmobarPP host = xmobarPP
    { ppCurrent = xmobarColor "#3399ff" "" . wrap " " " "
    , ppHidden  = xmobarColor "#dddddd" "" . wrap " " " "
    , ppHiddenNoWindows = xmobarColor "#777777" "" . wrap " " " "
    , ppUrgent  = xmobarColor "#ff0000" "" . wrap " " " "
    , ppSep     = "     "
    , ppLayout  = xmobarColor "#aaaaaa" "" . wrap "·" "·"
    , ppTitle   = xmobarColor "#ffffff" "" . shorten w
    }
  where w = case host
              of Laptop _ -> 45
                 Desktop  -> 60

-- my custom keybindings.
myKeys status host = myKeymap host (myConfig status host)

myKeymap host conf =
    -- mod-[1..],       Switch to workspace N
    -- mod-shift-[1..], Move client to workspace N
    -- mod-ctrl-[1..],  Switch to workspace N on other screen
    [ (m ++ "M-" ++ [k], f i)                                   -- (0)
        | (i, k) <- zip (XMonad.workspaces conf) "1234567890-=[]\\" -- (0)
        , (f, m) <- [ (windows . W.view, "")                    -- (0a)
                    , (windows . W.shift, "S-")
                    , (\ws -> nextScreen >> (windows . W.view $ ws), "C-")
                    ]
    ]

    ++
    [ ("M-S-x", spawnShell)
    , ("M-S-e", spawnEditor)
    , ("M-a", spawnSelected defaultGSConfig [ "code"
                                            , "vivaldi-snapshot", "firefox", "dolphin", "FreeFileSync"
                                            , "zeal", "qterminal"])
    , ("M-g", goToSelected $ myGSConfig myColorizer )
    --, ("M-g",   promptedGoto)
    , ("M-S-g", promptedShift)
    , ("M-S-C-g", workspacePrompt myXPConfig $ withAll' . W.shiftWin)

    , ("M-S-r", WN.renameWorkspace def)

    -- sink floating windows
    , ("M-t", withFocused $ windows . W.sink)

    -- rotate workspaces.
    , ("M-C-<R>",   nextWS )                                    -- (16)
    , ("M-C-<L>",   prevWS )                                    --  "
    , ("M-S-<R>",   shiftToNext )                               --  "
    , ("M-S-<L>",   shiftToPrev )                               --  "
    , ("M-S-C-<R>", shiftToNext >> nextWS )                     --  "
    , ("M-S-C-<L>", shiftToPrev >> prevWS )                     --  "
    , ("M-<R>",     moveTo Next HiddenNonEmptyWS)               --  "
    , ("M-<L>",     moveTo Prev HiddenNonEmptyWS)               --  "

    -- expand/shrink windows
    , ("M-r k", sendMessage MirrorExpand)
    , ("M-r j", sendMessage MirrorShrink)
    , ("M-r h", sendMessage Shrink)
    , ("M-r l", sendMessage Expand)

    -- switch to previous workspace
    , ("M-z", toggleWS)
    , ("M-S-z", killAll >> liftIO (threadDelay 2000) >> moveTo Prev HiddenNonEmptyWS)
    , ("C-M-<Delete>", replicateM 10 (killAll >> moveTo Prev HiddenNonEmptyWS) >> spawn "poweroff")

    -- dynamic workspace bindings
    --, ("M-n", addWorkspacePrompt myXPConfig)
    , ("M-C-r", removeWorkspace)
    , ("M-C-S-r", killAll >> removeWorkspace)

    -- move between screens
    , ("M-s", nextScreen)
    , ("M-w", swapNextScreen)
    , ("M-e", shiftNextScreen)

    -- lock the screen with xscreensaver
    , ("M-S-l", spawn "xscreensaver-command -lock")

    -- scratchpads
    , ("M-[ h", namedScratchpadAction scratchpads "ghci")
    , ("M-[ p", namedScratchpadAction scratchpads "ipython")
    , ("M-[ j", namedScratchpadAction scratchpads "node")
    , ("M-[ g", namedScratchpadAction scratchpads "psql")
    , ("M-[ z", namedScratchpadAction scratchpads "zeal")
    , ("M-[ a", namedScratchpadAction scratchpads "alsamixer")
    , ("M-[ c", namedScratchpadAction scratchpads "capture")
    , ("M-[ t", namedScratchpadAction scratchpads "htop")
    -- alsamixer and xkill
    , ("M-c v", spawn "terminal -e alsamixer")
    , ("M-c k", spawn "xkill")

    -- window navigation keybindings.
    , ("S-C-<R>", sendMessage $ Swap R)                         --  "
    , ("S-C-<L>", sendMessage $ Swap L)                         --  "
    , ("S-C-<U>", sendMessage $ Swap U)                         --  "
    , ("S-C-<D>", sendMessage $ Swap D)                         --  "

    -- toggles: fullscreen, flip x, flip y, mirror, no borders
    , ("M-C-<Space>", sendMessage $ Toggle NBFULL)              -- (14)
    , ("M-C-x",       sendMessage $ Toggle REFLECTX)            -- (14,13)
    , ("M-C-y",       sendMessage $ Toggle REFLECTY)            -- (14,13)
    , ("M-C-m",       sendMessage $ Toggle MIRROR)              --  "
    , ("M-S-f",       sendMessage $ ToggleStruts)

    , ("M-p", shellPrompt myXPConfig)
    , ("M-o", docPrompt myXPConfig)

    -- some random utilities.
    , ("M-q", spawn "xmonad --recompile && xmonad --restart")
    , ("M-c s", spawn "scrot -s $HOME/`date +%Y-%m-%d_%H:%M:%S`.png")
    ]

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig                                    -- (23)
    { fgColor = "#a8a3f7"
    , bgColor = "#3f3c6d"
    }

-- Set up a customized manageHook (rules for handling windows on
--   creation)
myManageHook :: ManageHook                                      -- (0)
myManageHook =
  composeAll
      $ [ className =? c <||> isDialog --> doCenterFloat | c <- myFloats ]
      ++ [ title =~ "Call with" --> doCenterFloat ]
      ++ [ className =? name --> doShift ws | (name, ws) <- myWindows ]
    where myFloats = [ "Volume"
                     , "XClock"
                     , "wicd-client.py"
                     , "Ssh-askpass-fullscreen"
                     ]
          myWindows = [ ("Okular",       "doc")
                      , ("Gwenview",     "doc") ]
          contains :: (Eq a) => [a] -> [a] -> Bool
          contains p = any (isPrefixOf p) . tails
          q =~ x = fmap (contains x) q

-- specify a custom layout hook.
myLayoutHook = avoidStruts
             $ spacing 1
             -- navigate directionally rather than with mod-j/k
             -- $ configurableNavigation (navigateColor "#006666")

             -- ability to toggle between fullscreen, reflect x/y, no borders,
             -- and mirrored.
             $ mkToggle1 NBFULL
             $ mkToggle1 REFLECTX
             $ mkToggle1 REFLECTY
             $ mkToggle1 NOBORDERS
             $ mkToggle1 MIRROR
             $ smartBorders
             $ ThreeColMid 1 (10/100) (1/2)
               ||| ThreeCol 1 (10/100) (1/2)
               ||| HintedTile 1 (3/100) (1/2) TopLeft Wide
               ||| myTiled  -- resizable tall layout
               ||| Grid
               ||| Full


-- use ResizableTall instead of Tall, but still call it "Tall".
myTiled = named "Tall" $ ResizableTall 1 0.03 0.5 []

-- scratchpads
scratchpads =
  map f ["ghci", "node", "ipython", "htop"] ++
  [ NS "ocaml" "urxvtc -T ocaml -e rlwrap ocaml" (title =? "ocaml") doSPFloat
  , NS "psql" "urxvt -T psql -e psql -U postgres" (title =? "psql") doSPFloat
  , NS "zeal" "zeal" (className =? "zeal") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  , NS "stardict" "stardict" (className =? "Stardict") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  ]
  where
    urxvt prog = ("urxvt -T "++) . ((++) . head $ words prog) . (" -e "++) . (prog++) $ ""
    f s = NS s (urxvt s) (title =? s) (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    doTopFloat = customFloating $ W.RationalRect (1/3) 0 (1/3) (1/3)
    doTopLeftFloat = customFloating $ W.RationalRect 0 0 (1/3) (1/3)
    doTopRightFloat = customFloating $ W.RationalRect (2/3) 0 (1/3) (1/3)
    doBottomLeftFloat = customFloating $ W.RationalRect 0 (2/3) (1/3) (1/3)
    doBottomRightFloat = customFloating $ W.RationalRect (2/3) (2/3) (1/3) (1/3)
    doLeftFloat = customFloating $ W.RationalRect 0 0 (1/3) 1
    orgFloat = customFloating $ W.RationalRect (1/2) (1/2) (1/2) (1/2)
    doSPFloat = customFloating $ W.RationalRect (1/6) (1/6) (4/6) (4/6)



-- GridSelect color scheme
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
	(0x00,0x00,0x00) -- lowest inactive bg
	(0x60,0xA0,0xC0) -- highest inactive bg
	(0x34,0x75,0xAA) -- active bg
	(0xBB,0xBB,0xBB) -- inactive fg
  (0x00,0x00,0x00) -- active fg

myColorizerGreen = colorRangeFromClassName
    black            -- lowest inactive bg
    (0x70,0xFF,0x70) -- highest inactive bg
    black            -- active bg
    white            -- inactive fg
    white            -- active fg
  where black = minBound
        white = maxBound


myGSConfig colorizer = (buildDefaultGSConfig colorizer) { gs_cellheight = 30, gs_cellwidth = 100 }

-- Doc prompt
data Doc = Doc

instance XPrompt Doc where
    showXPrompt Doc = "okular "

docPrompt :: XPConfig -> X ()
docPrompt c = do
  files <- liftIO $ flatDir "$HOME" 2
  let files' = filter ((==".pdf") . takeExtension . fst) files
      cmds = map (\(f, fp) -> (f, spawn $ "okular " ++ fp)) files'
  docPromptC cmds c
    where
      flatDir :: FilePath -> Int -> IO [(FilePath, FilePath)]
      flatDir _    0     = return []
      flatDir base depth = do
                ok <- return . isDirectory =<< getFileStatus base
                if ok
                   then do
                     fs <- getDirectoryContents base
                     fs' <- mapM (flip flatDir (depth - 1)) (map (base </>) fs)
                     return $ (concat fs') ++ (map (\f -> (f, base </> f)) fs)
                   else do
                     return []

docPromptC :: [(String, X ())] -> XPConfig -> X ()
docPromptC commands c =
    mkXPrompt Doc c (mkComplFunFromList' (map fst commands)) $
        fromMaybe (return ()) . (`lookup` commands)
