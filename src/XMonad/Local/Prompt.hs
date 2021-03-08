{-# LANGUAGE LambdaCase #-}

-- |
--
-- Copyright:
--   This file is part of the package xmonadrc. It is subject to the
--   license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/xmonadrc
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the
--   terms contained in the LICENSE file.
--
-- License: BSD-3-Clause
--
-- XMonad.Prompt configuration and utilities.
module XMonad.Local.Prompt
  ( promptConfig,
    listCompFunc,
    aListCompFunc,

    -- * Web Applications
    WebApp (..),
    webAppURL,
    webAppCommand,
    webAppPrompt,
  )
where

import Control.Category ((>>>))
import Data.Char (toLower)
import Data.List (isInfixOf)
import Text.Printf (printf)
import Text.Read (readMaybe)
import XMonad.Core
import XMonad.Prompt

promptConfig :: XPConfig
promptConfig =
  def
    { position = CenteredAt (1 / 3) (1 / 2),
      height = 50,
      font = "xft:Fira Code:size=14",
      bgColor = "#262e3d",
      fgColor = "#eeeeee",
      fgHLight = "#ffffff",
      bgHLight = "#c50ed2",
      borderColor = "#0D1729",
      promptBorderWidth = 4,
      maxComplRows = Just 12,
      alwaysHighlight = False,
      promptKeymap = emacsLikeXPKeymap,
      searchPredicate = predicateFunction
    }

-- | Build a completion function for a list of strings using the
-- search predicate stored in the @XPConfig@.
listCompFunc :: XPConfig -> [String] -> String -> IO [String]
listCompFunc c xs s = return (filter (searchPredicate c s) xs)

-- | Like @listCompFunc@ but expects an association list.
aListCompFunc :: XPConfig -> [(String, a)] -> String -> IO [String]
aListCompFunc c xs = listCompFunc c (map fst xs)

-- | A case-insensitive substring predicate function.
predicateFunction :: String -> String -> Bool
predicateFunction x y = lc x `isInfixOf` lc y where lc = map toLower

-- | Web applications that I display in dedicated browser windows.
data WebApp
  = Outlook
  | Tweetdeck
  | GoogleMessages
  | Mattermost
  deriving (Show, Read, Enum, Bounded)

-- | Convert a 'WebApp' into a URL.
webAppURL :: WebApp -> String
webAppURL = \case
  Outlook -> "https://outlook.office365.com/mail/inbox"
  Tweetdeck -> "https://tweetdeck.twitter.com/"
  GoogleMessages -> "https://messages.google.com/web/conversations"
  Mattermost -> "https://chat.rfa.sc.gov/login"

-- | The full-screen browser to use.
webAppCommand :: WebApp -> String
webAppCommand = printf "browser --app='%s'" . webAppURL

-- | For prompting.
data WebAppPrompt = WebAppPrompt

instance XPrompt WebAppPrompt where
  showXPrompt WebAppPrompt = "Web App: "

-- | Prompt for a 'WebApp' and launch it.
webAppPrompt :: XPConfig -> X ()
webAppPrompt xpconfig = mkXPrompt WebAppPrompt xpconfig comp go
  where
    comp =
      listCompFunc
        xpconfig
        (map show (enumFrom minBound :: [WebApp]))
    go =
      readMaybe >>> \case
        Nothing -> pure ()
        Just webapp -> spawn (webAppCommand webapp)
