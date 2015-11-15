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
       ( switchTopic
       , names
       , asKey
       , viewPrevWS
       ) where

--------------------------------------------------------------------------------
import Control.Monad (unless)
import Data.List (sort, union, stripPrefix)
import qualified Data.Map as Map
import System.Directory (setCurrentDirectory, getHomeDirectory)
import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.TopicSpace (TopicConfig (..), Topic, Dir)
import qualified XMonad.Actions.TopicSpace as TopicSpace
import XMonad.Local.Prompt
import XMonad.Prompt
import XMonad.Prompt.Workspace
import qualified XMonad.StackSet as StackSet

--------------------------------------------------------------------------------
-- | A simplification of the @TopicConfig@ type that is focused more
-- on a single topic.  A list of these types is then converted into a
-- single @TopicConfig@.
data TopicDetails = TopicDetails
  { topicName :: Topic
  , topicDir  :: Dir
  , topicHook :: X ()
  }

--------------------------------------------------------------------------------
topics :: [TopicDetails]
topics =
    [ TopicDetails { topicName = "scratch"
                   , topicDir  = "~/"
                   , topicHook = return ()
                   }

    , TopicDetails { topicName = "mail"
                   , topicDir  = "~/"
                   , topicHook = do spawn "e -cs irc"
                                    spawn "e -cs gnus"
                   }

    , TopicDetails { topicName = "xmonad"
                   , topicDir  = "~/core/xmonad"
                   , topicHook = return ()
                   }
    ]

--------------------------------------------------------------------------------
-- | Names of my workspaces.
names :: [WorkspaceId]
names = ["scratch"]

--------------------------------------------------------------------------------
mkTopicConfig :: [TopicDetails] -> TopicConfig
mkTopicConfig []      = def
mkTopicConfig details = def
    { topicDirs          = directories
    , topicActions       = actions
    , defaultTopicAction = const $ return ()
    , defaultTopic       = topicName (head details)
    , maxTopicHistory    = 10
    }
  where
    directory d = (topicName d, topicDir d)
    directories = Map.fromList (map directory details)

    action d = (topicName d, topicHook d)
    actions  = Map.fromList (map action details)

--------------------------------------------------------------------------------
-- | Prompt for a topic to switch to, then switch to it.  Creates the
-- workspace if it doesn't already exist.
switchTopic :: X ()
switchTopic = topicPrompt $ \name ->
  case Map.lookup name (topicDirs tc) of
    Just dir            -> go name dir
    Nothing | null name -> return ()
            | otherwise -> go name "~"
  where
    -- Shortcut for the topic config.
    tc = mkTopicConfig topics

    -- Replace @~@ with the path to the home directory.
    expandHome home dir = case stripPrefix "~" dir of
      Nothing -> dir
      Just xs -> home ++ xs

    go topic dir = do
      home <- io getHomeDirectory
      removeEmptyWorkspace
      addWorkspace topic
      catchIO (setCurrentDirectory $ expandHome home dir)
      TopicSpace.switchTopic tc topic

--------------------------------------------------------------------------------
topicPrompt :: (String -> X ()) -> X ()
topicPrompt f = do
  ws <- map StackSet.tag <$> gets (StackSet.workspaces . windowset)
  let ns = sort $ map topicName topics `union` ws
  mkXPrompt (Wor "") promptConfig (mkComplFunFromList' ns) f

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
  let hs = StackSet.hidden ws
  unless (null hs) (windows . StackSet.view . StackSet.tag $ head hs)
