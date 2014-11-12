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
import XMonad.Core hiding (keys)
import XMonad.Layout
import XMonad.Operations
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- Package: xmonad-contrib.
import XMonad.Actions.CycleSelectedLayouts (cycleThroughLayouts)
import XMonad.Actions.OnScreen (onlyOnScreen)
import XMonad.Actions.PhysicalScreens (onPrevNeighbour, onNextNeighbour)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.UpdatePointer (PointerPosition(..), updatePointer)
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import qualified XMonad.Layout.BoringWindows as Boring
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Maximize (maximizeRestore)
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window (windowPromptGoto)
import XMonad.Prompt.XMonad (xmonadPrompt)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Paste (sendKey)

--------------------------------------------------------------------------------
-- Local modules.
import XMonad.Local.Music (albumPrompt, radioPrompt)
import qualified XMonad.Local.Prompt as Local
import XMonad.Local.Workspaces (asKey, viewPrevWS)

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
            , workspaceKeys
            , layoutKeys
            , screenKeys
            , appKeys
            , musicKeys
            ]

--------------------------------------------------------------------------------
-- | Change focus and update the mouse pointer.
changeFocus :: X () -> X ()
changeFocus f = f >> updatePointer (Relative 0.98 0.01)

--------------------------------------------------------------------------------
-- Specifically manage my prefix key (C-z), and for controlling XMonad.
--
-- TODO: spawn "xmonad --recompile && xmonad --restart"
-- TODO: ("C-z C-z", return ()) -- FIXME: replace with jumpToPrevWS
baseKeys :: XConfig Layout -> [(String, X ())]
baseKeys _ =
  [ ("C-z z",   sendKey controlMask xK_z) -- Send C-z to application.
  , ("C-z C-g", return ()) -- No-op to cancel the prefix key.
  , ("C-z g",   return ()) -- Same as above.
  , ("C-z S-q", io exitSuccess)
  , ("C-z x",   xmonadPrompt Local.promptConfig)
  ]

--------------------------------------------------------------------------------
-- Window focusing, swapping, and other actions.
windowKeys :: XConfig Layout -> [(String, X ())]
windowKeys _ =
  [ ("C-z f",     changeFocus $ sendMessage $ Go R)
  , ("C-z b",     changeFocus $ sendMessage $ Go L)
  , ("C-z n",     changeFocus $ sendMessage $ Go D)
  , ("C-z p",     changeFocus $ sendMessage $ Go U)
  , ("C-z C-v",   changeFocus Boring.focusDown)
  , ("C-z M1-v",  changeFocus Boring.focusUp)
  , ("C-z S-f",   changeFocus $ sendMessage $ Swap R)
  , ("C-z S-b",   changeFocus $ sendMessage $ Swap L)
  , ("C-z S-n",   changeFocus $ sendMessage $ Swap D)
  , ("C-z S-p",   changeFocus $ sendMessage $ Swap U)
  , ("C-z S-v",   changeFocus $ windows W.swapDown)
  , ("C-z m",     changeFocus Boring.focusMaster)
  , ("C-z S-m",   promote) -- Promote current window to master.
  , ("C-z S-t",   withFocused $ windows . W.sink) -- Tile window.
  , ("C-z M-b",   Boring.markBoring)
  , ("C-z M-S-b", Boring.clearBoring)
  , ("C-z w",     windowPromptGoto Local.promptConfig)
  , ("C-z S-k",   kill) -- Kill the current window.
  , ("M--",       sendMessage Shrink)
  , ("M-=",       sendMessage Expand)
  , ("M-S--",     sendMessage MirrorShrink)
  , ("M-S-=",     sendMessage MirrorExpand)
  , ("C-z -",     sendMessage $ IncMasterN (-1))
  , ("C-z =",     sendMessage $ IncMasterN 1)
  ]

--------------------------------------------------------------------------------
-- Keys for manipulating workspaces.
workspaceKeys :: XConfig Layout -> [(String, X ())]
workspaceKeys c = workspaceMovementKeys c ++ workspaceOtherKeys c

--------------------------------------------------------------------------------
-- Keys for moving windows between workspaces and switching workspaces.
workspaceMovementKeys :: XConfig Layout -> [(String, X ())]
workspaceMovementKeys c = do
  (name,   key)    <- zip (workspaces c) (map asKey $ workspaces c)
  (prefix, action) <- actions
  return (prefix ++ key, changeFocus $ windows (action name))
  where actions = [ -- Bring workspace N to the current screen.
                    ("C-z ",   W.greedyView)
                  , -- Move the current window to workspace N.
                    ("C-z S-", W.shift)
                  , -- Force workspace N to the second screen.
                    ("C-z C-", onlyOnScreen 1)
                  , -- Force workspace N to the third screen.
                    ("C-z M-", onlyOnScreen 2)
                  ]

--------------------------------------------------------------------------------
-- Other operations on workspaces not covered in 'workspaceMovementKeys'.
workspaceOtherKeys :: XConfig Layout -> [(String, X ())]
workspaceOtherKeys _ =
  [ ("C-z l",   changeFocus viewPrevWS)
  , ("C-z C-z", changeFocus viewPrevWS) -- TODO: Remove duplicate binding.
  ]

--------------------------------------------------------------------------------
-- Layout switching and manipulation.
layoutKeys :: XConfig Layout -> [(String, X ())]
layoutKeys _ =
  [ ("C-z <Space>",   withFocused (sendMessage . maximizeRestore))
  , ("C-z C-<Space>", cycleThroughLayouts ["Tall", "RTall"])
  , ("C-z S-<Space>", sendMessage $ JumpToLayout "Full")
  , ("C-z s",         sendMessage ToggleStruts)
  , ("M-0",           sendMessage $ JumpToLayout "Tall")
  , ("M-1",           sendMessage $ JumpToLayout "Full")
  , ("M-2",           sendMessage $ JumpToLayout "2Col")
  , ("M-3",           sendMessage $ JumpToLayout "3Col")
  , ("M-4",           sendMessage $ JumpToLayout "MTall")
  ]

--------------------------------------------------------------------------------
-- Keys to manipulate screens (actual physical monitors).
screenKeys :: XConfig Layout -> [(String, X ())]
screenKeys _ =
  [ ("C-z C-f",   changeFocus $ onNextNeighbour W.view)
  , ("C-z C-b",   changeFocus $ onPrevNeighbour W.view)
  , ("M-<F11>",   spawn "xbacklight -dec 10")
  , ("M-<F12>",   spawn "xbacklight -inc 10")
  , ("M-S-<F11>", spawn "xbacklight -set 10")
  , ("M-S-<F12>", spawn "xbacklight -set 80")
  ]

--------------------------------------------------------------------------------
-- Keys for launching applications.
appKeys :: XConfig Layout -> [(String, X ())]
appKeys c =
  [ ("C-z t",     spawn $ terminal c)
  , ("C-z C-t",   spawn "mlclient --fontsize=18")
  , ("C-z M-t",   spawn "mlclient -b '#fdf6e3' -f '#002b36' --fontsize=18")
  , ("M-l",       spawn "i3lock -dc 444444")
  , ("<Print>",   spawn "screenshot.sh root")
  , ("M-<Print>", spawn "screenshot.sh window")
  , ("M-<Space>", shellPrompt Local.promptConfig)

    -- Laptops and keyboards with media/meta keys.
  , ("<XF86WebCam>",         spawn "tptoggle.sh") -- Weird.
  , ("<XF86TouchpadToggle>", spawn "tptoggle.sh")
  , ("M-<F6>",               spawn "tptoggle.sh")
  ]

--------------------------------------------------------------------------------
-- Keys for controlling music and volume.
musicKeys :: XConfig Layout -> [(String, X ())]
musicKeys _ =
  [ ("M-<F1>",  spawn "mpc-pause")
  , ("M-<F2>",  spawn "mpc prev")
  , ("M-<F3>",  spawn "mpc next")
  , ("M-<F4>",  spawn "mpc clear")
  , ("M4-<F1>", spawn "amixer set Master toggle")
  , ("M4-<F2>", spawn "amixer set Master 5%-")
  , ("M4-<F3>", spawn "amixer set Master 5%+")

    -- Keys for my laptop and keyboards with media keys.
  , ("M-<XF86AudioMute>",        spawn "mpc-pause")
  , ("M-<XF86AudioLowerVolume>", spawn "mpc prev")
  , ("M-<XF86AudioRaiseVolume>", spawn "mpc next")
  , ("<XF86AudioMute>",          spawn "amixer set Master toggle")
  , ("<XF86AudioLowerVolume>",   spawn "amixer set Master 5%-")
  , ("<XF86AudioRaiseVolume>",   spawn "amixer set Master 5%+")

    -- Prompt to change radio stations.
  , ("M4-<Space>",     radioPrompt Local.promptConfig)
  , ("C-z M4-<Space>", albumPrompt Local.promptConfig)
  ]
