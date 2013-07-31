--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Workspace configuration and utilities.
module XMonad.Local.Workspaces
       ( names
       , hidden
       , hiddenName
       , asKey
       , viewPrevWS
       ) where

--------------------------------------------------------------------------------
import Control.Monad (unless)
import XMonad
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- | Names of my workspaces. I have two sets of workspaces:
--
--  * Primary: 1 through 9 and 0 (i.e. the number row on a keyboard)
--  * Secondary: F1 through F12
names :: [WorkspaceId]
names = map show primary ++ map (("F" ++) . show) secondary
  where primary   = [1..9] ++ [0] :: [Int]
        secondary = [1..12]       :: [Int]

--------------------------------------------------------------------------------
-- | Names of workspaces that should be hidden from display when they
-- have no windows in them.
hidden :: [WorkspaceId]
hidden = filter (\(x:_) -> x == 'F') names

--------------------------------------------------------------------------------
-- | Returns a string that should be shown for the name of the
-- workspace when it doesn't contain any windows and is not a visible
-- workspace (i.e. the @ppHiddenNoWindows@ field in the @PP@ record).
hiddenName :: WorkspaceId -> String
hiddenName ('F':_) = ""
hiddenName x       = x

--------------------------------------------------------------------------------
-- | Helper function to translate workspace names into key names for
-- using in @EZConfig@ key bindings.
asKey :: String -> String
asKey ('F':xs) = "<F" ++ xs ++ ">" -- Function key (i.e. "F1" -> "<F1>")
asKey x        = x                 -- Any other key.

--------------------------------------------------------------------------------
-- | Toggle between the current and previous workspaces.
viewPrevWS :: X ()
viewPrevWS = do
  ws <- gets windowset
  let hs = W.hidden ws
  unless (null hs) (windows . W.view . W.tag $ head hs)
