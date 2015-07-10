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
       , runPromptConfig
       , listCompFunc
       , aListCompFunc
       ) where

--------------------------------------------------------------------------------
-- XMonad contrib (Prompt)
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)

--------------------------------------------------------------------------------
promptConfig :: XPConfig
promptConfig = def
  { position        = Bottom
  , font            = "xft:dejavu sans mono:size=9"
  , alwaysHighlight = True
  , promptKeymap    = emacsLikeXPKeymap
  , searchPredicate = fuzzyMatch
  , sorter          = fuzzySort
  }

--------------------------------------------------------------------------------
runPromptConfig :: XPConfig
runPromptConfig = promptConfig
  { alwaysHighlight  = False
  }

--------------------------------------------------------------------------------
-- | Build a completion function for a list of strings using the
-- search predicate stored in the @XPConfig@.
listCompFunc :: XPConfig -> [String] -> String -> IO [String]
listCompFunc c xs s = return (filter (searchPredicate c s) xs)

--------------------------------------------------------------------------------
-- | Like @listCompFunc@ but expects an association list.
aListCompFunc :: XPConfig -> [(String, a)] -> String -> IO [String]
aListCompFunc c xs s = listCompFunc c (map fst xs) s
