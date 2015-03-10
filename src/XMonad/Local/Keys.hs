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
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation (Direction (..), nextMatch)
import XMonad.Actions.Navigation2D
import XMonad.Actions.OnScreen (onlyOnScreen)
import XMonad.Actions.PhysicalScreens (onPrevNeighbour, onNextNeighbour)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.TagWindows
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Hooks.UrgencyHook (focusUrgent)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Hidden (hideWindow, popOldestHiddenWindow)
import XMonad.Layout.Maximize (maximizeRestore)
import XMonad.Layout.ResizableTile
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
import XMonad.Local.Layout (selectLayoutByName)

--------------------------------------------------------------------------------
-- General purpose resize commands.
data GPResize = Expand_L
              | Expand_R
              | Expand_U
              | Expand_D
              | Shrink_L
              | Shrink_R
              | Shrink_U
              | Shrink_D

--------------------------------------------------------------------------------
-- Actions for tagging windows.
data TagAction = ToggleTag String
               | FocusTag String
               | AddTagAndJump String

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
  [ ("M4-<Tab>",  changeFocus $ windows W.focusDown)
  , ("C-z l",     changeFocus $ nextMatch History (return True))
  , ("C-z f",     changeFocus $ windowGo R True)
  , ("C-z b",     changeFocus $ windowGo L True)
  , ("C-z n",     changeFocus $ windowGo D True)
  , ("C-z p",     changeFocus $ windowGo U True)
  , ("C-z C-f",   changeFocus $ windowGo R True)
  , ("C-z C-b",   changeFocus $ windowGo L True)
  , ("C-z C-n",   changeFocus $ windowGo D True)
  , ("C-z C-p",   changeFocus $ windowGo U True)
  , ("C-z o",     changeFocus $ goToSelected gridSelectConf)
  , ("C-z S-f",   changeFocus $ windowSwap R True)
  , ("C-z S-b",   changeFocus $ windowSwap L True)
  , ("C-z S-n",   changeFocus $ windowSwap D True)
  , ("C-z S-p",   changeFocus $ windowSwap U True)
  , ("C-z m",     changeFocus $ windows W.focusMaster)
  , ("C-z S-m",   changeFocus promote) -- Promote current window to master.
  , ("C-z S-t",   changeFocus $ withFocused $ windows . W.sink) -- Tile window.
  , ("C-z w",     changeFocus $ windowPromptGoto Local.promptConfig)
  , ("C-z S-k",   kill) -- Kill the current window.
  , ("C-z u",     changeFocus $ focusUrgent)
  , ("M--",       changeFocus $ sendResize Expand_L)
  , ("M-=",       changeFocus $ sendResize Shrink_L)
  , ("M-S--",     changeFocus $ sendResize Expand_U)
  , ("M-S-=",     changeFocus $ sendResize Shrink_U)
  , ("M-9",       changeFocus $ sendResize Shrink_R)
  , ("M-0",       changeFocus $ sendResize Expand_R)
  , ("M-S-9",     changeFocus $ sendResize Expand_D)
  , ("M-S-0",     changeFocus $ sendResize Shrink_D)
  , ("C-z r",     changeFocus $ sendMessage Rotate)
  , ("C-z -",     changeFocus $ sendMessage $ IncMasterN (-1))
  , ("C-z =",     changeFocus $ sendMessage $ IncMasterN 1)
  , ("C-z h",     withFocused hideWindow)
  , ("C-z M-h",   popOldestHiddenWindow)
  ]

--------------------------------------------------------------------------------
-- Navigate windows by using tags.
windowTagKeys :: XConfig Layout -> [(String, X ())]
windowTagKeys _ =
  [ ("C-z M-j",   setInteresting)
  , ("C-z j",     changeFocus (focusDownTaggedGlobal interestingWindowTag))
  ] ++ numberedTags
  where
    -- Removes the interesting tag from all windows, then sets it on
    -- the current window.
    setInteresting :: X ()
    setInteresting = do
      withTaggedGlobal interestingWindowTag (delTag interestingWindowTag)
      withFocused (addTag interestingWindowTag)

    -- A window tag to use for the currently interesting window.
    interestingWindowTag :: String
    interestingWindowTag = "interesting"

    numberedTags :: [(String, X ())]
    numberedTags = do
      key              <- map show ([1 .. 8] :: [Int])
      (prefix, action) <- numberedTemplate
      return (prefix ++ key, action key)

    numberedTemplate :: [(String, String -> X ())]
    numberedTemplate =
      [ ("M-",   withFocused . performTagAction . FocusTag)
      , ("M-S-", withFocused . performTagAction . AddTagAndJump)
      , ("M-C-", withFocused . performTagAction . ToggleTag)
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
  where actions = [ -- Bring workspace N to the current screen or make
                    -- it the focused workspace if on another screen.
                    ("C-z ",   W.view)
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
  [ ("C-z C-z", changeFocus viewPrevWS)
  ]

--------------------------------------------------------------------------------
-- Layout switching and manipulation.
layoutKeys :: XConfig Layout -> [(String, X ())]
layoutKeys _ =
  [ ("C-z <Space>",   withFocused (sendMessage . maximizeRestore))
  , ("C-z C-<Space>", selectLayoutByName gridSelectConf)
  , ("C-z s",         sendMessage ToggleStruts)
  ]

--------------------------------------------------------------------------------
-- Keys to manipulate screens (actual physical monitors).
screenKeys :: XConfig Layout -> [(String, X ())]
screenKeys _ =
  [ ("M-<Tab>",   changeFocus $ onNextNeighbour W.view)
  , ("M-S-<Tab>", changeFocus $ onPrevNeighbour W.view)
  , ("M-<F12>",   spawn "xbacklight -inc 10")
  , ("M-S-<F11>", spawn "xbacklight -set 10")
  , ("M-S-<F12>", spawn "xbacklight -set 80")
  ]

--------------------------------------------------------------------------------
-- Keys for launching applications.
appKeys :: XConfig Layout -> [(String, X ())]
appKeys c =
  [ ("C-z t",     spawn $ terminal c)
  , ("C-z C-t",   spawn $ terminal c ++ " -name BigTerm")
  , ("C-z M-t",   spawn $ terminal c ++ " -name BigTermP")
  , ("M-l",       spawn "i3lock -dc 444444")
  , ("<Print>",   spawn "screenshot.sh root")
  , ("M-<Print>", spawn "screenshot.sh window")
  , ("M-<Space>", shellPrompt Local.promptConfig)

    -- Laptops and keyboards with media/meta keys.
  , ("<XF86WebCam>",         spawn "tptoggle.sh") -- Weird.
  , ("<XF86TouchpadToggle>", spawn "tptoggle.sh")
  , ("M-<F6>",               spawn "tptoggle.sh")
  , ("M-<F10>",              spawn "screens.sh")
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

--------------------------------------------------------------------------------
sendResize :: GPResize -> X ()
sendResize movement = do
  winset <- gets windowset
  let ld = description . W.layout . W.workspace . W.current $ winset

  case (ld, movement) of
    ("BSP", Expand_L) -> sendMessage (ExpandTowards L)
    ("BSP", Expand_R) -> sendMessage (ExpandTowards R)
    ("BSP", Expand_U) -> sendMessage (ExpandTowards U)
    ("BSP", Expand_D) -> sendMessage (ExpandTowards D)
    ("BSP", Shrink_L) -> sendMessage (ShrinkFrom L)
    ("BSP", Shrink_R) -> sendMessage (ShrinkFrom R)
    ("BSP", Shrink_U) -> sendMessage (ShrinkFrom U)
    ("BSP", Shrink_D) -> sendMessage (ShrinkFrom D)
    (_,     Expand_L) -> sendMessage Shrink
    (_,     Expand_R) -> sendMessage Expand
    (_,     Expand_U) -> sendMessage MirrorShrink
    (_,     Expand_D) -> sendMessage MirrorExpand
    (_,     Shrink_L) -> sendMessage Expand
    (_,     Shrink_R) -> sendMessage Shrink
    (_,     Shrink_U) -> sendMessage MirrorExpand
    (_,     Shrink_D) -> sendMessage MirrorShrink

--------------------------------------------------------------------------------
performTagAction :: TagAction -> Window -> X ()
performTagAction action win = case action of
  ToggleTag tag     -> toggleWindowTag tag win
  FocusTag tag      -> changeFocus (focusDownTaggedGlobal tag)
  AddTagAndJump tag -> addTag tag win >> performTagAction (FocusTag tag) win

--------------------------------------------------------------------------------
toggleWindowTag :: String -> Window -> X ()
toggleWindowTag tag win = do
  tagged <- hasTag tag win
  (if tagged then delTag else addTag) tag win

--------------------------------------------------------------------------------
-- | Keys for moving around in GridSelect.
gridSelectNavKeys :: TwoD a (Maybe a)
gridSelectNavKeys = makeXEventhandler (shadowWithKeymap gsKeys gsSearch)
  where
    gsKeys = M.fromList
      [ ((controlMask, xK_g),      cancel)
      , ((0, xK_Return),           select)
      , ((controlMask, xK_Return), select)
      , ((0, xK_BackSpace),        gsErase      >> gridSelectNavKeys)
      , ((controlMask, xK_b),      move (-1, 0) >> gridSelectNavKeys)
      , ((controlMask, xK_f),      move (1,  0) >> gridSelectNavKeys)
      , ((controlMask, xK_n),      move (0,  1) >> gridSelectNavKeys)
      , ((controlMask, xK_p),      move (0, -1) >> gridSelectNavKeys)
      ]

    -- Unknown keys fall through to search.
    gsSearch (_,s,_) = transformSearchString (++ s) >> gridSelectNavKeys
    gsErase = transformSearchString (\s -> if (s == "") then "" else init s)

--------------------------------------------------------------------------------
gridSelectConf :: HasColorizer a => GSConfig a
gridSelectConf = def { gs_navigate = gridSelectNavKeys
                     }
