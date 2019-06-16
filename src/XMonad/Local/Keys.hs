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
import System.Directory
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Package: xmonad.
import XMonad hiding (keys)
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- Package: xmonad-contrib.
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleSelectedLayouts (cycleThroughLayouts)
import XMonad.Actions.DynamicProjects (switchProjectPrompt, lookupProject, switchProject)
import XMonad.Actions.GroupNavigation (Direction (..), nextMatch)
import XMonad.Actions.Navigation2D
import XMonad.Actions.PhysicalScreens (onNextNeighbour, onPrevNeighbour)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.RotSlaves (rotSlavesUp, rotSlavesDown)
import XMonad.Actions.SwapPromote (swapHybrid)
import XMonad.Actions.TagWindows (addTag, delTag, withTagged)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Hooks.UrgencyHook (focusUrgent)
import XMonad.Layout.LayoutBuilder (IncLayoutN(..))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing (incWindowSpacing, decWindowSpacing, toggleWindowSpacingEnabled)
import XMonad.Layout.ToggleLayouts (ToggleLayout(..))
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window (WindowPrompt(..), windowPrompt, windowMultiPrompt, allWindows, wsWindows)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad (namedScratchpadAction)

--------------------------------------------------------------------------------
-- Local modules.
import XMonad.Local.Layout (selectLayoutByName)
import XMonad.Local.Music (radioPrompt)
import qualified XMonad.Local.Prompt as Local
import XMonad.Local.Tagging
import XMonad.Local.Workspaces (asKey, viewPrevWS, scratchPads)

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
    addAction (key, action) = (key, action >> updatePointer (0.75, 0.25) (0, 0))

--------------------------------------------------------------------------------
-- Specifically manage my prefix key (C-z), and for controlling XMonad.
baseKeys :: XConfig Layout -> [(String, X ())]
baseKeys _ =
  [ ("M-x r",       restartIntoDebugging)
  , ("M-x <Space>", messageMenu Local.promptConfig)
  ]

--------------------------------------------------------------------------------
-- Window focusing, swapping, and other actions.
windowKeys :: XConfig Layout -> [(String, X ())]
windowKeys _ =
  -- Focusing Windows:
  [ ("M-;",     nextMatch History (return True))
  , ("M-w k",   windows W.focusUp)
  , ("M-w j",   windows W.focusDown)
  , ("M-u",     focusUrgent)
  , ("M-o",     windowPromptGoto')
  , ("M-C-o",   windowPromptGoto)
  , ("M-w c",   windowPrompt Local.promptConfig BringCopy allWindows)
  , ("M-j",     windowGo D True)
  , ("M-k",     windowGo U True)
  , ("M-l",     windowGo R True)
  , ("M-h",     windowGo L True)
  , ("M-C-m",   windows W.focusMaster)

  -- Moving Windows:
  , ("M-M1-l",  windowSwap R False)
  , ("M-M1-h",  windowSwap L False)
  , ("M-M1-j",  windowSwap D False)
  , ("M-M1-k",  windowSwap U False)
  , ("M-<U>",   rotSlavesUp)
  , ("M-<D>",   rotSlavesDown)
  , ("M-m",     whenX (swapHybrid False) promote) -- Promote current window to master.

  -- Resizing Windows:
  , ("M-C-h",   sendMessage Shrink)
  , ("M-C-l",   sendMessage Expand)
  , ("M-C-j",   sendMessage MirrorShrink)
  , ("M-C-k",   sendMessage MirrorExpand)
  , ("M-w -",   sendMessage $ IncMasterN (-1))
  , ("M-w S-=", sendMessage $ IncMasterN 1)

  -- Window Layers and Killing and Yanking:
  , ("M-w t",   withFocused $ windows . W.sink) -- Tile window.
  , ("M-q",     kill1) -- Kill the current window.
  , ("M-b",     killWindowToBury)
  , ("M-v",     yankWindowFromBury)
  ]

--------------------------------------------------------------------------------
-- Navigate windows by using tags.
windowTagKeys :: XConfig Layout -> [(String, X ())]
windowTagKeys _ =
  [ ("M-/",   tagPrompt Local.promptConfig)
  , ("M-a",   primaryJumpTagUp)
  , ("M-s",   secondaryJumpTagUp)
  , ("M-t a", addFocusTag)
  , ("M-t d", rmFocusTag)
  , ("M-t j", tagPrompt' Local.promptConfig [SetJumpTag])
  , ("M-t r", rmFocusTagAll >> addFocusTag)
  ] ++ numberedTags
  where
    addFocusTag :: X ()
    addFocusTag = do withFocused (addTag "focus")
                     sendMessage (IncLayoutN 0)

    rmFocusTag :: X ()
    rmFocusTag = do withFocused (delTag "focus")
                    sendMessage (IncLayoutN 0)

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
  [ ("M-'",       viewPrevWS)
  , ("M-<Space>", switchProjectPrompt  Local.promptConfig)
  , ("M-f",       lookupProject "agenda"   >>= maybe (return ()) switchProject)
  , ("M-g",       lookupProject "browsers" >>= maybe (return ()) switchProject)
  ]

--------------------------------------------------------------------------------
-- Layout switching and manipulation.
layoutKeys :: XConfig Layout -> [(String, X ())]
layoutKeys c =
  [ ("M-<Backspace>", selectLayoutByName Local.promptConfig)
  , ("M-w <Esc>",     setLayout (layoutHook c)) -- Reset to default layout.
  , ("M-S-1",         sendMessage ToggleLayout)
  , ("M-S-8",         cycleThroughLayouts ["Auto", "Focus"])
  , ("M-w s",         toggleWindowSpacingEnabled)
  , ("M-w M-s",       sendMessage ToggleStruts)
  , ("M-C-S-=",       incWindowSpacing 5)
  , ("M-C--",         decWindowSpacing 5)
  ]

--------------------------------------------------------------------------------
-- Keys to manipulate screens (actual physical monitors).
screenKeys :: XConfig Layout -> [(String, X ())]
screenKeys _ =
  [ ("M-S-0", onNextNeighbour def W.view)
  , ("M-S-9", onPrevNeighbour def W.view)
  , ("M-\\",  screenSwap L True)
  ]

--------------------------------------------------------------------------------
-- Keys for launching applications.
appKeys :: XConfig Layout -> [(String, X ())]
appKeys c =
  [ ("M-<Return>", spawn (terminal c))
  , ("M-<Esc>",    shellPrompt Local.promptConfig)
  , ("M-e",        emacs)
  , ("M-p",        spawn "rofi-pass.sh")
  , ("M-S-6",      namedScratchpadAction scratchPads "emacs")
  ]

--------------------------------------------------------------------------------
-- Keys for controlling music and volume.
musicKeys :: XConfig Layout -> [(String, X ())]
musicKeys _ =
    [ ("M-r", radioPrompt Local.promptConfig)
    ]

--------------------------------------------------------------------------------
-- | Start an Emacs server for the current workspace.
emacs :: X ()
emacs = do
  name <- gets (W.tag . W.workspace . W.current . windowset)
  spawn ("e -cs " ++ name)

--------------------------------------------------------------------------------
-- | Restart XMonad but instead of starting the XMonad in @PATH@,
-- start a debugging version built out of my development tree.
restartIntoDebugging :: X ()
restartIntoDebugging = do
  home <- io getHomeDirectory
  restart (home </> "src/rc/xmonadrc/dist/build/xmonadrc/xmonadrc")  True

--------------------------------------------------------------------------------
windowPromptGoto :: X ()
windowPromptGoto = windowMultiPrompt Local.promptConfig modes
  where modes = [(Goto, allWindows), (Goto, wsWindows)]

--------------------------------------------------------------------------------
windowPromptGoto' :: X ()
windowPromptGoto' = windowMultiPrompt Local.promptConfig modes
  where modes = [(Goto, wsWindows), (Goto, allWindows)]

--------------------------------------------------------------------------------
-- | A menu of less frequently used actions:
data MessageMenu = MessageMenu

instance XPrompt MessageMenu where
  showXPrompt MessageMenu = "X Action: "

messageMenu :: XPConfig -> X ()
messageMenu conf =
  mkXPrompt MessageMenu conf (Local.aListCompFunc conf actions) go

  where
    go :: String -> X ()
    go selected =
      case lookup selected actions of
        Nothing   -> return ()
        Just a    -> a

    actions :: [ (String, X ()) ]
    actions = [ ("IncLayoutN",    sendMessage (IncLayoutN 1))
              , ("DecLayoutN",    sendMessage (IncLayoutN (-1)))
              , ("IncMasterN",    sendMessage (IncMasterN 1))
              , ("DecMasterN",    sendMessage (IncMasterN (-1)))
              , ("ToggleStruts",  sendMessage ToggleStruts)
              , ("ToggleSpacing", toggleWindowSpacingEnabled)
              ]
