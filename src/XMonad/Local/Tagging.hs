{-# LANGUAGE DeriveDataTypeable #-}

--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | A tagging system for manipulating windows and workspaces.
--
-- This module is a simple wrapper around 'CopyWindows' and 'TagWindows'.
--
-- It adds some extensible state to XMonad to track something called
-- the "primary" and "secondary" jump tag.
--
-- The idea behind the primary jump tag is that you manually associate
-- a tag name with a key binding.  Issuing that key binding will jump
-- to the next window with the stored tag.
--
-- The secondary tag is automatically set.  It's the last tag selected
-- when adding a tag to a window or jumping to a tag from a tag
-- selection prompt.
module XMonad.Local.Tagging
  ( TagPromptMode (..)
  , tagPrompt
  , tagPrompt'
  , toggleTagOnCurrentWindow
  , setPrimaryJumpTag
  , primaryJumpTagDown
  , primaryJumpTagUp
  , secondaryJumpTagDown
  , secondaryJumpTagUp
  , focusTag
  , focusTag'
  , bringTaggedWindowsHere
  , deleteTaggedWindowsFromHere
  , killWindowToBury
  , yankWindowFromBury
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad (when)
import Data.List (find, nub)
import Data.Maybe (listToMaybe)
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.TagWindows hiding (TagPrompt, tagPrompt)
import XMonad.Prompt
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

--------------------------------------------------------------------------------
-- | Special tags that have key bindings to jump directly to the next
-- window.  The first tag @String@ is known as the "Primary Jump Tag",
-- and the second is known as the "Secondary Jump Tag".
--
-- The primary jump tag is manually set by the user.  The secondary
-- jump tag is automatically set to the last tag used.
data JumpTags = JumpTags !String !String
              deriving (Typeable, Read, Show)

instance ExtensionClass JumpTags where
  initialValue = JumpTags "browser" "browser"
  extensionType = PersistentExtension

--------------------------------------------------------------------------------
data TagPromptMode = FocusTag
                   | ToggleTag
                   | DeleteTagInWorkspace
                   | DeleteTagGlobaly
                   | BringTaggedWindows
                   | DeleteTaggedWindows
                   | SetJumpTag

data TagPrompt = TagPrompt TagPromptMode [String] (String -> String -> Bool)

--------------------------------------------------------------------------------
instance XPrompt TagPrompt where

  ------------------------------------------------------------------------------
  showXPrompt (TagPrompt mode _ _) =
    case mode of
      FocusTag             -> "Jump to next window tagged: "
      ToggleTag            -> "Add or remove window tag: "
      DeleteTagInWorkspace -> "Delete tag from workspace: "
      DeleteTagGlobaly     -> "Delete tag globally: "
      BringTaggedWindows   -> "Bring windows with tag: "
      DeleteTaggedWindows  -> "Delete windows with tag: "
      SetJumpTag           -> "Set jump tag: "

  ------------------------------------------------------------------------------
  completionFunction (TagPrompt _ tags predicate) =
    \s -> return . filter (predicate s) $ tags

  ------------------------------------------------------------------------------
  modeAction (TagPrompt mode _ _) buf auto = do
    let tag = if null auto then buf else auto

    case mode of
      FocusTag             -> focusTag tag
      ToggleTag            -> toggleTagOnCurrentWindow tag
      DeleteTagInWorkspace -> withTagged tag (delTag tag)
      DeleteTagGlobaly     -> withTaggedGlobal tag (delTag tag)
      BringTaggedWindows   -> bringTaggedWindowsHere tag
      DeleteTaggedWindows  -> deleteTaggedWindowsFromHere tag
      SetJumpTag           -> setPrimaryJumpTag tag

--------------------------------------------------------------------------------
buryTag :: String
buryTag = "bury"

--------------------------------------------------------------------------------
tagPrompt :: XPConfig -> X ()
tagPrompt c = tagPrompt' c modes
  where modes = [ FocusTag
                , ToggleTag
                , DeleteTagInWorkspace
                , DeleteTagGlobaly
                , BringTaggedWindows
                , DeleteTaggedWindows
                , SetJumpTag
                ]

--------------------------------------------------------------------------------
tagPrompt' :: XPConfig -> [TagPromptMode] -> X ()
tagPrompt' c ms = do
    tags <- tagComplList

    let predicate = searchPredicate c
        modes = map (\m -> XPT $ TagPrompt m tags predicate) ms

    mkXPromptWithModes modes c
  where
    -- Stolen from: XMonad.Actions.TagWindows
    tagComplList :: X [String]
    tagComplList = gets (concat . map (W.integrate' . W.stack) . W.workspaces . windowset) >>=
        mapM getTags >>= return . nub . concat

--------------------------------------------------------------------------------
-- | Get the recorded tags.
jumpTags :: X (String, String)
jumpTags = do
  JumpTags primary secondary <- XS.get
  return (primary, secondary)

--------------------------------------------------------------------------------
-- | Given a tag name, toggle it on the current window.  If adding a
-- tag, records that tag name as the secondary jump tag.
toggleTagOnCurrentWindow :: String -> X ()
toggleTagOnCurrentWindow = go where
  go :: String -> X ()
  go tag = withFocused (update tag)

  update :: String -> Window -> X ()
  update tag win = do
    tagged <- hasTag tag win

    if tagged
      then delTag tag win
      else do (primary, _) <- jumpTags
              XS.put (JumpTags primary tag)
              addTag tag win

--------------------------------------------------------------------------------
-- | Prompt for a tag name then record that tag as the current "Jump Tag".
setPrimaryJumpTag :: String -> X ()
setPrimaryJumpTag primary = do
  (_, secondary) <- jumpTags
  XS.put $ JumpTags primary secondary

--------------------------------------------------------------------------------
-- | Focus the next window with the primary jump tag.
primaryJumpTagDown :: X ()
primaryJumpTagDown = focusDownTaggedGlobal . fst =<< jumpTags

--------------------------------------------------------------------------------
-- | Focus the previous window with the primary jump tag.
primaryJumpTagUp :: X ()
primaryJumpTagUp = focusUpTaggedGlobal . fst =<< jumpTags

--------------------------------------------------------------------------------
-- | Focus the next window with the secondary jump tag.
secondaryJumpTagDown :: X ()
secondaryJumpTagDown = focusDownTaggedGlobal . snd =<< jumpTags

--------------------------------------------------------------------------------
-- | Focus the previous window with the secondary jump tag.
secondaryJumpTagUp :: X ()
secondaryJumpTagUp = focusUpTaggedGlobal . snd =<< jumpTags

--------------------------------------------------------------------------------
-- | Given a tag name, jump to the next window with that name.  Also
-- records the chosen tag name as the secondary jump tag.
focusTag :: String -> X ()
focusTag = go where
  go :: String -> X ()
  go tag = do
    (primary, _) <- jumpTags
    XS.put (JumpTags primary tag)
    focusUpTaggedGlobal tag

--------------------------------------------------------------------------------
-- | Focus a window with the given tag without updating the secondary
-- jump tag.
focusTag' :: String -> X ()
focusTag' = focusUpTaggedGlobal

--------------------------------------------------------------------------------
-- | "Copy" windows with the given tag to the current workspace.
bringTaggedWindowsHere :: String -> X ()
bringTaggedWindowsHere tag = withTaggedGlobal tag go where
  go :: Window -> X ()
  go win = do
    ws <- gets (W.tag . W.workspace . W.current . windowset)
    windows (copyWindow win ws)

--------------------------------------------------------------------------------
-- | Given a tag name, "un-copy" all windows on the current workspace
-- with that tag.  If the windows are present on another workspace
-- they will be removed from the current workspace.  Otherwise they
-- are left alone.
deleteTaggedWindowsFromHere :: String -> X ()
deleteTaggedWindowsFromHere tag = withTagged tag go where
  go :: Window -> X ()
  go win = do
    ss <- gets windowset

    when (W.member win (notWindow win ss)) $
      windows (notWindow win)

  notWindow :: (Eq a) => a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
  notWindow win = W.modify Nothing (W.filter (/= win))

--------------------------------------------------------------------------------
-- | "Kill" a window by sending it to a bury workspace.
killWindowToBury :: X ()
killWindowToBury = do
  addHiddenWorkspace buryTag
  windows (W.shift buryTag)

--------------------------------------------------------------------------------
-- | Yank a window from the bury workspace to the current workspace.
yankWindowFromBury :: X ()
yankWindowFromBury = do
  here <- gets (W.tag . W.workspace . W.current . windowset)
  wses <- gets (W.workspaces . windowset)

  let win' = do workspace <- find (\ws -> W.tag ws == buryTag) wses
                stack <- W.stack workspace
                listToMaybe (reverse $ W.integrate stack)

  case win' of
    Nothing  -> return ()
    Just win -> do windows (W.shiftWin here win)
                   removeEmptyWorkspaceByTag buryTag
