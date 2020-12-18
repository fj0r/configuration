{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad hiding ((|||), Tall)
import Data.Monoid
import System.Exit


import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Control.Monad
import Data.Monoid

import XMonad.Util.Cursor
import XMonad.Layout.Mosaic
import XMonad.Layout.Circle
import qualified XMonad.Layout.LayoutCombinators as L
import XMonad.Layout.Spacing
import qualified XMonad.Actions.Search as S
import XMonad.Hooks.DynamicLog
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad
-- import XMonad.Prompt.Eval
import XMonad.Actions.UpdatePointer
import XMonad.Prompt.Man
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.ManageDocks
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Layout.Fullscreen as F
import XMonad.Layout.Spiral
import XMonad.Layout.HintedTile
import XMonad.Actions.WindowBringer
import XMonad.Actions.CycleWS
import XMonad.Actions.Search
import XMonad.Actions.Warp
import XMonad.Actions.TopicSpace
import XMonad.Actions.ShowText
import System.Process
import XMonad.Actions.Promote
import XMonad.Hooks.EwmhDesktops as E
import Data.List
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutHints
import Text.Regex.TDFA ((=~~))
import Control.Monad.Trans
import TopicDSL

myEditor = "/home/eklerks/scripts/emacs.vim"

like :: XMonad.Query String -> String -> XMonad.Query Bool
like q x = isInfixOf x `fmap` q

-- | Regex query

regex :: XMonad.Query String -> String -> XMonad.Query Bool
regex q x = ( =~~ x) =<< q


toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

dzenC :: LayoutClass l Window
     => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
dzenC conf = statusBar ("dzen2 " ++ flags) dzenPP toggleStrutsKey conf
 where
    fg      = "'#a8a3f7'" -- n.b quoting
    bg      = "'#3f3c6d'"
    flags   = "-e 'onstart=lower' -w 400 -ta r -fg " ++ fg ++ " -bg " ++ bg

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
--
myBorderWidth   = 0

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#00ff00"
myFocusedBorderColor = "#ff0000"

workspaceKeys = [xK_1..xK_9] <> [xK_0]

myXPConfig = greenXPConfig
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

  [
    -- launch a terminal
      ((modm .|. shiftMask, xK_Return), currentTopic >>= \q -> spawnShell ( myTopicConfig q) q)
    -- Switch between layouts
    , ((modm .|. controlMask, xK_1), sendMessage $ L.JumpToLayout "Tall")
    , ((modm .|. controlMask, xK_2), sendMessage $ L.JumpToLayout "Wide")
    , ((modm .|. controlMask, xK_3), sendMessage $ L.JumpToLayout "Full")
    , ((modm .|. controlMask, xK_4), sendMessage $ L.JumpToLayout "Mirror")
    , ((modm .|. controlMask, xK_5), sendMessage $ L.JumpToLayout "Mosaic")
    , ((modm .|. controlMask, xK_6), sendMessage $ L.JumpToLayout "Circle")
    , ((modm .|. controlMask, xK_u), spawn "ruby /home/edgar/remote.rb nsfw")
    , ((modm .|. controlMask, xK_r), spawn "ruby /home/edgar/remote.rb sfw")
    , ((modm .|. controlMask, xK_n), spawn "ruby /home/edgar/remote.rb")
    , ((modm .|. controlMask, xK_p), spawn "zsh /home/eklerks/scripts/switch-screen.sh")
    -- xmonadPrompt
    , ((modm .|. controlMask, xK_x), xmonadPrompt myXPConfig)
    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")
    -- navigate screens
    , ((modm, xK_Left), prevWS)
    , ((modm, xK_Right), nextWS)
    , ((modm, xK_Down), shiftToPrev)
    , ((modm, xK_Up), shiftToNext)
    , ((modm .|. controlMask, xK_o), ( flashText def ((toRational 1/2)) "hello" ) >> windows (W.swapUp))
    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modm .|. shiftMask, xK_m ), manPrompt myXPConfig)
    , ((modm .|. shiftMask, xK_s), S.promptSearchBrowser myXPConfig "/usr/bin/firefox" mySearchEngine)
    , ((modm .|. shiftMask, xK_g), gotoMenu)-- windowPromptGoto defaultXPConfig)
    , ((modm .|. shiftMask, xK_b), bringMenu)-- windowPromptBring defaultXPConfig
    -- close) focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm, xK_s), setDefaultCursor xC_spider)
    , ((modm, xK_y), setDefaultCursor xC_pirate)


     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), promote)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm .|. controlMask, xK_h), hideWindow)
    , ((modm .|. shiftMask, xK_h), unhideAll)

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- eval prompt
  --  , ((modm .|. controlMask              ,xK_e), evalPrompt  defaultEvalConfig defaultXPConfig)
    , ((modm           , xK_KP_Add), spawn "amixer -c 0 set Master 2dB+")
    , ((modm           , xK_KP_Subtract), spawn "amixer -c 0 set Master 2dB-")
    , ((modm .|. shiftMask, xK_l), spawn "/home/eklerks/scripts/xtrlock")

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    <>


    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) workspaceKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    <>

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- e <-> w
    -- r <-> w
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust f )
        | (key, sc) <- [xK_e, xK_w, xK_r] `zip` [0..]
        , (f, m) <- [(gotoSpare, 0), (windows . W.shift, shiftMask)]]
  -- Topic spaces keys
   <>
  [ ((modm .|. shiftMask             , xK_n     ), currentTopic >>=  \q -> spawnShell ( myTopicConfig q) q) -- %! Launch terminal
  , ((modm .|. shiftMask              , xK_a     ),  currentTopicAction ( myTopicConfig "home"))
  , ((modm              , xK_g     ), promptedGoto)
  , ((modm .|. shiftMask, xK_g     ), promptedShift)
  , ((modm, xK_F1), manPrompt $ def {fgColor = "green", bgColor="black"})
  ]

hideWindow :: X ()
hideWindow = withWindowSet $ \ws -> do
           let screen = W.current ws
           let workspace = W.workspace $ screen
           case W.stack workspace of
             Nothing -> return ()
             Just windows -> do
               let currentWindow = W.focus windows
               hide currentWindow
           return ()
unhideAll :: X ()
unhideAll = withWindowSet $ \ws -> do
                    let windows = W.allWindows ws
                    forM_ windows $ \w -> do
                          reveal w


lastTopic :: X Topic
lastTopic = do xs <- getLastFocusedTopics
               flashText def (toRational 1/2) (show xs)
               case xs of
                 [] -> return (defaultTopic ( myTopicConfig "home"))
                 (x:xs) -> return x
evalTopicDescription :: TopicDescription -> [(Topic, Dir, X ())]
evalTopicDescription (TD xs) = let p = worker xs in p
                 where worker (x:xs) = let (tn, td, ta) = getTriple x
                                       in (tn, td, ta (myTopicConfig tn) td tn) : worker xs
                       worker [] = []

createTopicConfig :: Topic -> TopicDescription -> TopicConfig
createTopicConfig dt td = let p1 (a,b,c) = a
                              p12 (a,b,c) = (a,b)
                              p13 (a,b,c) = (a,c)
                              xs = evalTopicDescription td

                          in def {
                               topicDirs = M.fromList (p12 <$> xs),
                               defaultTopicAction = \t -> defaultAction myTopicConfig t "~",
                               defaultTopic = dt,
                               topicActions = M.fromList (p13 <$> xs)

                            }

myTopicConfig :: String -> TopicConfig
myTopicConfig tn = createTopicConfig tn topicDescription

myTopics :: Topics
myTopics = (\(x,y,z) -> x) <$> evalTopicDescription topicDescription

gotoSpare :: Topic -> X ()
gotoSpare topic = do
          windows $ W.view topic
          wins <- gets $ W.integrate' . W.stack . W.workspace . W.current . windowset
          when (null wins) $ topicAction (myTopicConfig topic) topic
          updatePointer (0.5, 0.5) (0,0)

currentTopic :: X Topic
currentTopic = gets (W.tag . W.workspace . W.current . windowset)
goto :: Topic -> X ()
goto tn = switchTopic ( myTopicConfig tn) tn

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $


    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)),
    ((modm .|. shiftMask, button1), (\w -> focus w >> Flex.mouseWindow Flex.linear w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--
-- take Fibonacci as AZZ
mosaicLayout = mosaic 5 fib
    where
     fib = reverse $ take 5 $ fib'
        where fib' = 1 : 2 : zipWith (+) fib' (tail fib')

myLayout = avoidStruts $ layoutHints $ fullscreenFull $ spacing 2 $ tiled Tall L.||| tiled Wide L.||| renamed [Replace "Mirror"] (Mirror (tiled Tall)) L.||| Full L.||| mosaicLayout L.||| Circle
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = HintedTile nmaster delta ratio Center

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> ( doFloat *> doShift "sources" )
    , (className `like` "XMathematica") <||> (appName  `like` "Wolfram") <||> (appName `like` "Drawing Tools")  --> doFloat
    , appName =? "desktop_window" --> doIgnore
    , appName =? "kdesktop"       --> doIgnore
    , appName =? "rhinote" --> doFloat
    , className =? "Firefox" <||> className =? "firefox"  --> doShift "browser"
    , className =? "pidgin" <||> className =? "Pidgin" --> doShift "6"
    ] <+> manageDocks <+> fullscreenManageHook
------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
-- hintsEventHook
myEventHook = ewmhDesktopsEventHook <+> F.fullscreenEventHook <+> E.fullscreenEventHook <+> docksEventHook -- promoteWarp

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = dynamicLogWithPP xmobarPP

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = void $ do
        liftIO $ system "zsh /home/eklerks/.xmonad.zsh &"
        setDefaultCursor xC_spider

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--

topicDescription = fillActions myTopicConfig $(fromDisk )



main = do
--    system("setxkbmap -layout us")
     xconfig <- xmobar defaults


     xmonad $ ewmh xconfig

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--fi
-- No need to modify this.
--


defaults = def {
      -- simple stuff
        terminal           = "urxvtcd",
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myTopics, -- myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }



-- Search function
--

mySearchEngine = (wikipedia !> alpha !> mathworld !> hoogle !> hackage !> scholar !> prefixAware google)
