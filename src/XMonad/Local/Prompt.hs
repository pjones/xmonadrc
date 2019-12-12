--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | XMonad.Prompt configuration and utilities.
module XMonad.Local.Prompt
       ( promptConfig
       , listCompFunc
       , aListCompFunc
       ) where

--------------------------------------------------------------------------------
-- Library Imports
import Data.Char (toLower)
import Data.List (isInfixOf)

--------------------------------------------------------------------------------
-- XMonad contrib (Prompt)
import XMonad.Prompt

--------------------------------------------------------------------------------
promptConfig :: XPConfig
promptConfig = def
  { position          = CenteredAt (1/3) (1/2)
  , height            = 50
  , font              = "xft:Fira Code:size=14"
  , bgColor           = "#262e3d"
  , fgColor           = "#eeeeee"
  , fgHLight          = "#ffffff"
  , bgHLight          = "#c50ed2"
  , borderColor       = "#0D1729"
  , promptBorderWidth = 4
  , maxComplRows      = Just 12
  , alwaysHighlight   = False
  , promptKeymap      = emacsLikeXPKeymap
  , searchPredicate   = predicateFunction
  }

--------------------------------------------------------------------------------
-- | Build a completion function for a list of strings using the
-- search predicate stored in the @XPConfig@.
listCompFunc :: XPConfig -> [String] -> String -> IO [String]
listCompFunc c xs s = return (filter (searchPredicate c s) xs)

--------------------------------------------------------------------------------
-- | Like @listCompFunc@ but expects an association list.
aListCompFunc :: XPConfig -> [(String, a)] -> String -> IO [String]
aListCompFunc c xs = listCompFunc c (map fst xs)

--------------------------------------------------------------------------------
-- | A case-insensitive substring predicate function.
predicateFunction :: String -> String -> Bool
predicateFunction x y = lc x `isInfixOf` lc y where lc = map toLower
