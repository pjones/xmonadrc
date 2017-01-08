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
import Graphics.X11.Xlib
import System.Exit (exitSuccess)

--------------------------------------------------------------------------------
-- Package: xmonad.
import XMonad hiding (keys)
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- Package: xmonad-contrib.
import XMonad.Actions.DynamicProjects (switchProjectPrompt, shiftToProjectPrompt, renameProjectPrompt)
import XMonad.Actions.GroupNavigation (Direction (..), nextMatch)
import XMonad.Actions.Navigation2D
import XMonad.Actions.PhysicalScreens (onPrevNeighbour, onNextNeighbour)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.TagWindows
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Hooks.UrgencyHook (focusUrgent)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Maximize (maximizeRestore)
import XMonad.Layout.ResizableTile
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window (windowPromptGoto, windowPromptBring)
import XMonad.Prompt.XMonad (xmonadPrompt)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Paste (sendKey)
import XMonad.Actions.CopyWindow (kill1)

--------------------------------------------------------------------------------
-- Local modules.
import XMonad.Local.Layout (selectLayoutByName)
import XMonad.Local.Music (radioPrompt)
import qualified XMonad.Local.Prompt as Local
import XMonad.Local.Tagging
import XMonad.Local.Workspaces (asKey, viewPrevWS)

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

--------------------------------------------------------------------------------
-- Join all the key maps into a single list and send it through @mkKeymap@.
keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys c = mkKeymap c (rawKeys c)

--------------------------------------------------------------------------------
-- | Access the unprocessed key meant to be fed into @mkKeymap@.
rawKeys :: XConfig Layout -> [(String, X ())]
rawKeys c = concatMap ($ c) keymaps where
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
-- | Change focus and update the mouse pointer.
changeFocus :: X () -> X ()
changeFocus f = f >> updatePointer (0.98, 0.01) (0, 0)

--------------------------------------------------------------------------------
-- Specifically manage my prefix key (C-z), and for controlling XMonad.
baseKeys :: XConfig Layout -> [(String, X ())]
baseKeys _ =
  [ ("C-z z",   sendKey controlMask xK_z) -- Send C-z to application.
  , ("C-z C-g", return ()) -- No-op to cancel the prefix key.
  , ("C-z g",   return ()) -- Same as above.
  , ("C-z q",   restart "xmonadrc" True)
  , ("C-z S-q", io exitSuccess)
  , ("C-z x",   xmonadPrompt Local.promptConfig)
  ]

--------------------------------------------------------------------------------
-- Window focusing, swapping, and other actions.
windowKeys :: XConfig Layout -> [(String, X ())]
windowKeys _ =
  [ ("C-z l",     changeFocus $ nextMatch History (return True))
  , ("C-z b",     changeFocus $ windows W.focusUp)
  , ("C-z f",     changeFocus $ windows W.focusDown)
  , ("C-z n",     changeFocus $ windowGo D True)
  , ("C-z p",     changeFocus $ windowGo U True)
  , ("C-z C-f",   changeFocus $ windowGo R True)
  , ("C-z C-b",   changeFocus $ windowGo L True)
  , ("C-z C-n",   changeFocus $ windowGo D True)
  , ("C-z C-p",   changeFocus $ windowGo U True)
  , ("C-z o",     changeFocus $ windowPromptGoto  Local.promptConfig)
  , ("C-z C-o",   changeFocus $ windowPromptBring Local.promptConfig)
  , ("C-z S-f",   changeFocus $ windowSwap R True)
  , ("C-z S-b",   changeFocus $ windowSwap L True)
  , ("C-z S-n",   changeFocus $ windowSwap D True)
  , ("C-z S-p",   changeFocus $ windowSwap U True)
  , ("C-z m",     changeFocus $ windows W.focusMaster)
  , ("C-z S-m",   changeFocus promote) -- Promote current window to master.
  , ("M-t",       changeFocus $ withFocused $ windows . W.sink) -- Tile window.
  , ("C-z S-k",   kill1) -- Kill the current window.
  , ("C-z u",     changeFocus focusUrgent)
  , ("M--",       changeFocus $ sendResize GPExpandL)
  , ("M-=",       changeFocus $ sendResize GPShrinkL)
  , ("M-S--",     changeFocus $ sendResize GPExpandU)
  , ("M-S-=",     changeFocus $ sendResize GPShrinkU)
  , ("M-M4--",    changeFocus $ sendResize GPShrinkR)
  , ("M-M4-=",    changeFocus $ sendResize GPExpandR)
  , ("M-M4-S--",  changeFocus $ sendResize GPExpandD)
  , ("M-M4-S-=",  changeFocus $ sendResize GPShrinkD)
  , ("C-z r",     changeFocus $ sendMessage Rotate)
  , ("C-z -",     changeFocus $ sendMessage $ IncMasterN (-1))
  , ("C-z =",     changeFocus $ sendMessage $ IncMasterN 1)
  , ("C-z C-k",   changeFocus   killWindowToBury)
  , ("C-z C-y",   changeFocus   yankWindowFromBury)
  ]

--------------------------------------------------------------------------------
-- Navigate windows by using tags.
windowTagKeys :: XConfig Layout -> [(String, X ())]
windowTagKeys _ =
  [ ("M-j",       setPrimaryJumpTag)
  , ("M-<Left>",  changeFocus primaryJumpTagDown)
  , ("M-<Right>", changeFocus primaryJumpTagUp)
  , ("C-z C-j",   changeFocus primaryJumpTagDown)
  , ("C-z j",     changeFocus secondaryJumpTagDown)
  , ("C-z t",     changeFocus selectAndFocusTag)
  , ("C-z M-t",   toggleTagOnCurrentWindow)
  , ("C-z M-S-t", deleteTag)
  , ("C-z c",     changeFocus bringTaggedWindowsHere)
  , ("C-z S-c",   changeFocus deleteTaggedWindowsFromHere)
  ] ++ numberedTags
  where
    numberedTags :: [(String, X ())]
    numberedTags = do
      key              <- map show ([0 .. 9] :: [Int]) ++
                          map (("F" ++) . show) ([1 .. 12] :: [Int])
      (prefix, action) <- numberedTemplate
      return (prefix ++ asKey key, action key)

    numberedTemplate :: [(String, String -> X ())]
    numberedTemplate =
      [ ("C-z ",   changeFocus . focusDownTaggedGlobal)
      , ("C-z M-", withFocused . addTag)
      ]

--------------------------------------------------------------------------------
-- Keys for manipulating workspaces.
workspaceKeys :: XConfig Layout -> [(String, X ())]
workspaceKeys c =
  [ ("C-z C-z", changeFocus viewPrevWS)
  , ("C-z w",   changeFocus $ switchProjectPrompt  Local.promptConfig)
  , ("C-z M-w", changeFocus $ renameProjectPrompt  Local.promptConfig)
  , ("C-z S-w", changeFocus $ shiftToProjectPrompt Local.promptConfig)
  , ("M-q",     changeFocus $ setLayout (layoutHook c))
  ]

--------------------------------------------------------------------------------
-- Layout switching and manipulation.
layoutKeys :: XConfig Layout -> [(String, X ())]
layoutKeys _ =
  [ ("C-z <Space>",   withFocused (sendMessage . maximizeRestore))
  , ("C-z C-<Space>", selectLayoutByName Local.promptConfig)
  , ("C-z s",         sendMessage ToggleStruts)
  ]

--------------------------------------------------------------------------------
-- Keys to manipulate screens (actual physical monitors).
screenKeys :: XConfig Layout -> [(String, X ())]
screenKeys _ =
  [ ("M-<Up>",    changeFocus $ onPrevNeighbour W.view)
  , ("M-<Down>",  changeFocus $ onNextNeighbour W.view)
  , ("C-z d",     changeFocus $ onNextNeighbour W.view)
  , ("M-<F11>",   spawn "xbacklight -dec 10")
  , ("M-<F12>",   spawn "xbacklight -inc 10")
  , ("M-S-<F11>", spawn "xbacklight -set 10")
  , ("M-S-<F12>", spawn "xbacklight -set 80")
  ]

--------------------------------------------------------------------------------
-- Keys for launching applications.
appKeys :: XConfig Layout -> [(String, X ())]
appKeys _ =
  [ ("M-<Return>", spawn "urxvtc")
  , ("M-l",        spawn "lockscreen.sh")
  , ("<Print>",    spawn "screenshot.sh root")
  , ("M-<Print>",  spawn "screenshot.sh window")
  , ("C-z C-e",    spawn "e -c") -- Start per-workspace Emacs.
  , ("M-<Space>",  shellPrompt Local.runPromptConfig)

    -- Laptops and keyboards with media/meta keys.
  , ("<XF86WebCam>",         spawn "tptoggle.sh") -- Weird.
  , ("<XF86TouchpadToggle>", spawn "tptoggle.sh")
  , ("M-<F6>",               spawn "tptoggle.sh")
  , ("M-<F10>",              spawn "xrandr-projector")
  ]

--------------------------------------------------------------------------------
-- Keys for controlling music and volume.
musicKeys :: XConfig Layout -> [(String, X ())]
musicKeys _ =
    [ ("M-<F1>",   playPause)
    , ("M-<F2>",   prevTrack)
    , ("M-<F3>",   nextTrack)
    , ("M-S-<F4>", clearPlaylist)
    , ("M4-<F1>",  audioMute)
    , ("M4-<F2>",  audioLower)
    , ("M4-<F3>",  audioRaise)

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

      -- Prompt to change radio stations.
    , ("M4-<Space>",     radioPrompt Local.promptConfig)
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
  let ld = description . W.layout . W.workspace . W.current $ winset

  case (ld, movement) of
    ("BSP", GPExpandL) -> sendMessage (ExpandTowards L)
    ("BSP", GPExpandR) -> sendMessage (ExpandTowards R)
    ("BSP", GPExpandU) -> sendMessage (ExpandTowards U)
    ("BSP", GPExpandD) -> sendMessage (ExpandTowards D)
    ("BSP", GPShrinkL) -> sendMessage (ShrinkFrom L)
    ("BSP", GPShrinkR) -> sendMessage (ShrinkFrom R)
    ("BSP", GPShrinkU) -> sendMessage (ShrinkFrom U)
    ("BSP", GPShrinkD) -> sendMessage (ShrinkFrom D)
    (_,     GPExpandL) -> sendMessage Shrink
    (_,     GPExpandR) -> sendMessage Expand
    (_,     GPExpandU) -> sendMessage MirrorShrink
    (_,     GPExpandD) -> sendMessage MirrorExpand
    (_,     GPShrinkL) -> sendMessage Expand
    (_,     GPShrinkR) -> sendMessage Shrink
    (_,     GPShrinkU) -> sendMessage MirrorExpand
    (_,     GPShrinkD) -> sendMessage MirrorShrink
