--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Key bindings.
module XMonad.Local.Keys (keys, rawKeys) where

--------------------------------------------------------------------------------
-- General Haskell Packages.
import qualified Data.Map as M
import Data.Version (showVersion)
import Graphics.X11.Xlib
import System.Directory

--------------------------------------------------------------------------------
-- Package: xmonad.
import XMonad hiding (keys)
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- Package: xmonad-contrib.
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.DynamicProjects (switchProjectPrompt)
import XMonad.Actions.GroupNavigation (Direction (..), nextMatch)
import XMonad.Actions.Navigation2D
import XMonad.Actions.PhysicalScreens (onNextNeighbour, onPrevNeighbour)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.TagWindows (addTag, delTag, withTagged)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Hooks.UrgencyHook (focusUrgent)
import XMonad.Layout.BinarySpacePartition hiding (Swap)
import XMonad.Layout.ComboP (PartitionWins(..))
import XMonad.Layout.Gaps (GapMessage(..))
import XMonad.Layout.LayoutCombinators (JumpToLayout(..))
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts (ToggleLayout(..))
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window (WindowPrompt(..), windowPrompt, windowMultiPrompt, allWindows, wsWindows)
import XMonad.Prompt.XMonad (xmonadPrompt)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad (namedScratchpadAction)

--------------------------------------------------------------------------------
-- Local modules.
import Paths_xmonadrc (version)
import XMonad.Local.Layout (selectLayoutByName)
import XMonad.Local.Music (radioPrompt)
import XMonad.Local.Prompt (promptConfig)
import qualified XMonad.Local.Prompt as Local
import XMonad.Local.Tagging
import XMonad.Local.Workspaces (asKey, viewPrevWS, scratchPads)

--------------------------------------------------------------------------------
-- General purpose resize commands.
data GPResize = GPExpandL
              | GPExpandR
              | GPExpandU
              | GPExpandD
              | GPShrinkL
              | GPShrinkR
              | GPShrinkU
              | GPShrinkD

data LayoutType = BSP    -- ^ Binary Space Partition
                | Other  -- ^ Other (generic) layout

--------------------------------------------------------------------------------
-- Join all the key maps into a single list and send it through @mkKeymap@.
keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys c = mkKeymap c (rawKeys c)

--------------------------------------------------------------------------------
-- | Access the unprocessed key meant to be fed into @mkKeymap@.
rawKeys :: XConfig Layout -> [(String, X ())]
rawKeys c = withUpdatePointer $ concatMap ($ c) keymaps where
  keymaps = [ baseKeys
            , windowKeys
            , windowTagKeys
            , workspaceKeys
            , layoutKeys
            , screenKeys
            , appKeys
            , musicKeys
            ]

--------------------------------------------------------------------------------
-- | Modify all keybindings so that after they finish their action the
-- mouse pointer is moved to the corner of the focused window.  This
-- is a bit of a hack to work around some issues I have with
-- @UpdatePointer@.
withUpdatePointer :: [(String, X ())] -> [(String, X ())]
withUpdatePointer = map addAction
  where
    addAction :: (String, X ()) -> (String, X ())
    addAction (key, action) = (key, action >> updatePointer (0.98, 0.01) (0, 0))

--------------------------------------------------------------------------------
-- Specifically manage my prefix key (C-z), and for controlling XMonad.
baseKeys :: XConfig Layout -> [(String, X ())]
baseKeys _ =
  [ ("M-g",           return ()) -- Same as above.
  , ("M-x M-r",       restart "xmonadrc" True)
  , ("M-x M-d",       restartIntoDebugging)
  , ("M-x M-<Space>", xmonadPrompt Local.promptConfig)
  ]

--------------------------------------------------------------------------------
-- Window focusing, swapping, and other actions.
windowKeys :: XConfig Layout -> [(String, X ())]
windowKeys _ =
  -- Focusing Windows:
  [ ("M-w M-l",   nextMatch History (return True))
  , ("M-w M-b",   windows W.focusUp)
  , ("M-w M-f",   windows W.focusDown)
  , ("M-w M-u",   focusUrgent)
  , ("M-w M-o",   windowPromptGoto)
  , ("M-w M-c",   windowPrompt Local.promptConfig BringCopy allWindows)
  , ("M-o",       windowPromptGoto')
  , ("M-n",       windowGo D True)
  , ("M-p",       windowGo U True)
  , ("M-f",       windowGo R True)
  , ("M-b",       windowGo L True)
  , ("M-w M-m",   windows W.focusMaster)

  -- Moving Windows:
  , ("M-m M-f",  windowSwap R False)
  , ("M-m M-b",  windowSwap L False)
  , ("M-m M-n",  windowSwap D False)
  , ("M-m M-p",  windowSwap U False)
  , ("M-m M-m",  promote) -- Promote current window to master.

  -- Resizing Windows:
  , ("M-<Left>",  sendResize GPExpandL)
  , ("M-<Right>", sendResize GPShrinkL)
  , ("M-<Up>",    sendResize GPExpandU)
  , ("M-<Down>",  sendResize GPShrinkU)
  , ("M-w -",     sendMessage $ IncMasterN (-1))
  , ("M-w =",     sendMessage $ IncMasterN 1)

  -- Window Layers and Killing and Yanking:
  , ("M-w M-t",   withFocused $ windows . W.sink) -- Tile window.
  , ("M-w M-k",   kill1) -- Kill the current window.
  , ("M-k",       killWindowToBury)
  , ("M-y",       yankWindowFromBury)
  ]

--------------------------------------------------------------------------------
-- Navigate windows by using tags.
windowTagKeys :: XConfig Layout -> [(String, X ())]
windowTagKeys _ =
  [ ("M-t M-<Space>", tagPrompt Local.promptConfig)
  , ("M-h",           secondaryJumpTagUp)
  , ("M-j",           primaryJumpTagUp)
  , ("M-t M-a",       addFocusTag)
  , ("M-t M-d",       rmFocusTag)
  , ("M-t M-j",       tagPrompt' Local.promptConfig [SetJumpTag])
  , ("M-t M-r",       rmFocusTagAll >> addFocusTag)
  ] ++ numberedTags
  where
    addFocusTag :: X ()
    addFocusTag = do withFocused (addTag "focus")
                     sendMessage PartitionWins

    rmFocusTag :: X ()
    rmFocusTag = do withFocused (delTag "focus")
                    sendMessage PartitionWins

    rmFocusTagAll :: X ()
    rmFocusTagAll = withTagged "focus" (delTag "focus")

    numberedTags :: [(String, X ())]
    numberedTags = do
      key              <- map show ([0 .. 9] :: [Int]) ++
                          map (("F" ++) . show) ([1 .. 12] :: [Int])
      (prefix, action) <- numberedTemplate
      return (prefix ++ asKey key, action key)

    numberedTemplate :: [(String, String -> X ())]
    numberedTemplate =
      [ ("M-",   focusTag')
      , ("M-t ", toggleTagOnCurrentWindow)
      ]

--------------------------------------------------------------------------------
-- Keys for manipulating workspaces.
workspaceKeys :: XConfig Layout -> [(String, X ())]
workspaceKeys _ =
  [ ("M-w M-w",   viewPrevWS)
  , ("M-<Space>", switchProjectPrompt  Local.promptConfig)
  ]

--------------------------------------------------------------------------------
-- Layout switching and manipulation.
layoutKeys :: XConfig Layout -> [(String, X ())]
layoutKeys c =
  [ ("M-l M-<Space>", selectLayoutByName Local.promptConfig)
  , ("M-l M-<Esc>",   setLayout (layoutHook c)) -- Reset to default layout.
  , ("M-l M-2",       sendMessage (JumpToLayout "2C"))
  , ("M-l M-3",       sendMessage (JumpToLayout "3C"))
  , ("M-l M-b",       sendMessage (JumpToLayout "BSP"))
  , ("M-l M-f",       sendMessage (JumpToLayout "Focus") >> sendMessage PartitionWins)
  , ("M-l M-l",       sendMessage (Toggle "Full"))
  , ("M-l M-t",       sendMessage (JumpToLayout "Tall"))
  , ("M-w M-g",       sendMessage ToggleGaps)
  , ("M-w M-r",       sendMessage Rotate)
  , ("M-w M-s",       sendMessage ToggleStruts)
  ]

--------------------------------------------------------------------------------
-- Keys to manipulate screens (actual physical monitors).
screenKeys :: XConfig Layout -> [(String, X ())]
screenKeys _ =
  [ ("M-s M-f",    onNextNeighbour W.view)
  , ("M-s M-b",    onPrevNeighbour W.view)
  , ("M-s M-s",    screenSwap L True)
  , ("M4-<F11>",   spawn "xbacklight -dec 10")
  , ("M4-<F12>",   spawn "xbacklight -inc 10")
  , ("M4-S-<F11>", spawn "xbacklight -set 10")
  , ("M4-S-<F12>", spawn "xbacklight -set 80")
  ]

--------------------------------------------------------------------------------
-- Keys for launching applications.
appKeys :: XConfig Layout -> [(String, X ())]
appKeys _ =
  [ ("M-<Return>",     spawn "urxvtc -e tmux-new-terminal")
  , ("M4-<Return>",    spawn "urxvtc -name BigTerm -e tmux-new-terminal")
  , ("M-M4-l",         spawn "lockscreen.sh")
  , ("M-<Print> M-r",  spawn "screenshot.sh root")
  , ("M-<Print> M-w",  spawn "screenshot.sh window")
  , ("M-e M-e",        spawn "e -c") -- Start per-workspace Emacs.
  , ("M-e M-r",        shellPrompt Local.promptConfig)

    -- Laptops and keyboards with media/meta keys.
  , ("<XF86WebCam>",         spawn "tptoggle.sh") -- Weird.
  , ("<XF86TouchpadToggle>", spawn "tptoggle.sh")
  , ("M4-<F6>",              spawn "tptoggle.sh")
  , ("M4-<F10>",             spawn "xrandr-projector")

    -- Scratch pads.
  , ("M-; M-c", namedScratchpadAction scratchPads "calc")
  , ("M-; M-p", namedScratchpadAction scratchPads "pass")
  , ("M-; M-t", namedScratchpadAction scratchPads "todoist")
  -- FIXME: , ("M-; M-;", closeAllNamedScratchpads scratchPads)
  ]

--------------------------------------------------------------------------------
-- Keys for controlling music and volume.
musicKeys :: XConfig Layout -> [(String, X ())]
musicKeys _ =
    [ ("M-S-1",  playPause)
    , ("M-S-2",  prevTrack)
    , ("M-S-3",  nextTrack)
    , ("M-S-4",  radioPrompt Local.promptConfig)
    , ("M-S-5",  clearPlaylist)
    , ("M-S-6",  audioMute)
    , ("M-S-7",  audioLower)
    , ("M-S-8",  audioRaise)

      -- Keys for my laptop and keyboards with media keys.
    , ("M-<XF86AudioMute>",        playPause)
    , ("M-<XF86AudioLowerVolume>", prevTrack)
    , ("M-<XF86AudioRaiseVolume>", nextTrack)
    , ("<XF86AudioPlay>",          playPause)
    , ("<XF86AudioPrev>",          prevTrack)
    , ("<XF86AudioNext>",          nextTrack)
    , ("<XF86AudioMute>",          audioMute)
    , ("<XF86AudioLowerVolume>",   audioLower)
    , ("<XF86AudioRaiseVolume>",   audioRaise)
    ]
  where
    playPause     = spawn "mpc-pause"
    nextTrack     = spawn "mpc next"
    prevTrack     = spawn "mpc prev"
    clearPlaylist = spawn "mpc clear"
    audioMute     = spawn "amixer set Master toggle"
    audioLower    = spawn "amixer set Master 5%-"
    audioRaise    = spawn "amixer set Master 5%+"

--------------------------------------------------------------------------------
sendResize :: GPResize -> X ()
sendResize movement = do
  winset <- gets windowset
  let lname = description . W.layout . W.workspace . W.current $ winset
      ltype = case lname of
              "BSP"         -> BSP
              "Focus"       -> BSP
              _             -> Other

  case (ltype, movement) of
    (BSP,   GPExpandL) -> sendMessage (ExpandTowards L)
    (BSP,   GPExpandR) -> sendMessage (ExpandTowards R)
    (BSP,   GPExpandU) -> sendMessage (ExpandTowards U)
    (BSP,   GPExpandD) -> sendMessage (ExpandTowards D)
    (BSP,   GPShrinkL) -> sendMessage (ShrinkFrom L)
    (BSP,   GPShrinkR) -> sendMessage (ShrinkFrom R)
    (BSP,   GPShrinkU) -> sendMessage (ShrinkFrom U)
    (BSP,   GPShrinkD) -> sendMessage (ShrinkFrom D)
    (Other, GPExpandL) -> sendMessage Shrink
    (Other, GPExpandR) -> sendMessage Expand
    (Other, GPExpandU) -> sendMessage MirrorShrink
    (Other, GPExpandD) -> sendMessage MirrorExpand
    (Other, GPShrinkL) -> sendMessage Expand
    (Other, GPShrinkR) -> sendMessage Shrink
    (Other, GPShrinkU) -> sendMessage MirrorExpand
    (Other, GPShrinkD) -> sendMessage MirrorShrink

--------------------------------------------------------------------------------
-- | Restart XMonad but instead of starting the XMonad in @PATH@,
-- start a debugging version built out of my development tree.
restartIntoDebugging :: X ()
restartIntoDebugging = do
  home <- io getHomeDirectory

  -- Path to my xmonad (as generated by `cabal new-build'):
  let path = foldl (\x y -> x ++ "/" ++ y) home
               [ "core/xmonadrc/dist-newstyle/build"
               , "xmonadrc-" ++ showVersion version
               , "build/xmonadrc/xmonadrc"
               ]

  exists <- io (doesFileExist path)

  if exists
    then restart path True
    else do io (putStrLn ("bad path: " ++ path))
            confirmPrompt promptConfig
              "xmonad.errors for bad path"
              (return ())

--------------------------------------------------------------------------------
windowPromptGoto :: X ()
windowPromptGoto = windowMultiPrompt Local.promptConfig modes
  where modes = [(Goto, allWindows), (Goto, wsWindows)]

--------------------------------------------------------------------------------
windowPromptGoto' :: X ()
windowPromptGoto' = windowMultiPrompt Local.promptConfig modes
  where modes = [(Goto, wsWindows), (Goto, allWindows)]
