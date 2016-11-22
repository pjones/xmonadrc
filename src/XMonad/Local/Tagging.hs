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
  ( toggleTagOnCurrentWindow
  , setPrimaryJumpTag
  , primaryJumpTagDown
  , primaryJumpTagUp
  , secondaryJumpTagDown
  , secondaryJumpTagUp
  , selectAndFocusTag
  , deleteTag
  , bringTaggedWindowsHere
  , deleteTaggedWindowsFromHere
  , killWindowToBury
  , yankWindowFromBury
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad (when)
import Data.List (find)
import Data.Maybe (listToMaybe)
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.TagWindows
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

--------------------------------------------------------------------------------
-- Local Imports:
import XMonad.Local.Prompt

--------------------------------------------------------------------------------
data JumpTags = JumpTags !String !String
              deriving (Typeable, Read, Show)

instance ExtensionClass JumpTags where
  initialValue = JumpTags "browser" "browser"
  extensionType = PersistentExtension

--------------------------------------------------------------------------------
buryTag :: String
buryTag = "bury"

--------------------------------------------------------------------------------
-- | Get the recorded tags.
jumpTags :: X (String, String)
jumpTags = do
  JumpTags primary secondary <- XS.get
  return (primary, secondary)

--------------------------------------------------------------------------------
-- | Prompt for a tag name, then toggle it on the current window.  If
-- adding a tag, records that tag name as the secondary jump tag.
toggleTagOnCurrentWindow :: X ()
toggleTagOnCurrentWindow = tagPrompt promptConfig go where
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
setPrimaryJumpTag :: X ()
setPrimaryJumpTag = do
  (_, secondary) <- jumpTags
  tagPrompt promptConfig (\primary -> XS.put $ JumpTags primary secondary)

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
-- | Prompt for a tag name then jump to the next window with that
-- name.  Also records the chosen tag name as the secondary jump tag.
selectAndFocusTag :: X ()
selectAndFocusTag = tagPrompt promptConfig go where
  go :: String -> X ()
  go tag = do
    (primary, _) <- jumpTags
    XS.put (JumpTags primary tag)
    focusDownTaggedGlobal tag

--------------------------------------------------------------------------------
-- | Prompt for a tag name and then remove it from all windows.
deleteTag :: X ()
deleteTag = tagPrompt promptConfig go where
  go :: String -> X ()
  go tag = withTaggedGlobal tag (delTag tag)

--------------------------------------------------------------------------------
-- | Prompt for a tag name and then "copy" those windows to the
-- current workspace.
bringTaggedWindowsHere :: X ()
bringTaggedWindowsHere = tagPrompt promptConfig (`withTaggedGlobal` go) where
  go :: Window -> X ()
  go win = do
    ws <- gets (W.tag . W.workspace . W.current . windowset)
    windows (copyWindow win ws)

--------------------------------------------------------------------------------
-- | Prompt for a tag name then "un-copy" all windows on the current
-- workspace with that tag.  If the windows are present on another
-- workspace they will be removed from the current workspace.
-- Otherwise they are left alone.
deleteTaggedWindowsFromHere :: X ()
deleteTaggedWindowsFromHere = tagPrompt promptConfig (`withTagged` go) where
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
