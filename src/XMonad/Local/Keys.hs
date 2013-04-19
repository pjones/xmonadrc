--------------------------------------------------------------------------------
-- | Key bindings.
module XMonad.Local.Keys (keys) where

--------------------------------------------------------------------------------
-- General Haskell Packages.
import qualified Data.Map as M
import Graphics.X11.Xlib
import System.Exit (ExitCode(..), exitWith)

--------------------------------------------------------------------------------
-- Package: xmonad.
import XMonad.Core (X, XConfig(layoutHook, terminal), Layout, io, spawn)
import XMonad.Operations
import qualified XMonad.StackSet as W
import XMonad.Layout (ChangeLayout(..))

--------------------------------------------------------------------------------
-- Package: xmonad-contrib.
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Paste (sendKey)
import XMonad.Actions.Promote (promote)
import XMonad.Layout.ToggleLayouts (ToggleLayout(..))

--------------------------------------------------------------------------------
keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys c = mkKeymap c $ baseKeys c

--------------------------------------------------------------------------------
baseKeys :: XConfig Layout -> [(String, X ())]
baseKeys c =
  [ -- Specifically manage my prefix key (C-z).
    ("C-z z",   sendKey controlMask xK_z) -- Send C-z to application.
  , ("C-z C-g", return ()) -- No-op to cancel the prefix key.
  , ("C-z g",   return ()) -- Same as above.

    -- Controlling the XMonad instance itself.
  , ("C-z S-q", io $ exitWith ExitSuccess)

    -- TODO: , ((0,         xK_q),  spawn "xmonad --recompile && xmonad --restart")
    -- TOOD: , ((0,         xK_x),  xmonadPrompt myXPConfig)

    -- Window focusing, swapping, and other actions.
  , ("C-z n",   windows W.focusDown)
  , ("C-z p",   windows W.focusUp)
  , ("C-z o",   windows W.focusDown)
  , ("C-z S-n", windows W.swapDown)
  , ("C-z S-p", windows W.swapUp)
  , ("C-z m",   windows W.focusMaster)
  , ("C-z S-m", promote) -- Promote current window to master.
  , ("C-z S-t", withFocused $ windows . W.sink) -- Tile window.
  , ("C-z S-k", kill) -- Kill the current window.
  , ("C-z S-b", markBoring)

    -- Window sizes, master windows, etc.
  , ("M-<L>",        sendMessage Shrink)
  , ("M-<R>",        sendMessage Expand)
  , ("M-<U>",        sendMessage MirrorShrink)
  , ("M-<D>",        sendMessage MirrorExpand)
  , ("M-<Subtract>", sendMessage $ IncMasterN (-1))
  , ("M-<Equal>",    sendMessage $ IncMasterN 1)

    -- Layout switching and manipulation.
  , ("C-z <Space>",   sendMessage ToggleLayout)
  , ("C-z C-<Space>", sendMessage NextLayout)
  , ("C-z S-<Space>", setLayout $ layoutHook c)
  , ("C-z s",         sendMessage ToggleStruts)

    -- TODO: , ((0,         xK_w),  windowPromptGoto myXPConfig)
    -- TODO: ("C-z C-z", return ()) -- FIXME: replace with jumpToPrevWS

    -- Spawning other applications (general).
  , ("C-z t",   spawn $ terminal c)
  , ("C-z C-t", spawn "urxvtc -name BigTerm")

    -- Music and volume.
  , ("M-<F1>",            spawn "mpc-pause")
  , ("M-<F2>",            spawn "mpc prev")
  , ("M-<F3>",            spawn "mpc next")
  , ("M4-<F1>",           spawn "amixer set Master toggle")
  , ("M4-<F2>",           spawn "amixer set Master 5%-")
  , ("M4-<F3>",           spawn "amixer set Master 5%+")
  , ("M-<XF86AudioMute>", spawn "mpc-pause")
  , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
  ]
