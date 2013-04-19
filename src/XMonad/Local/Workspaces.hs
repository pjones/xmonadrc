--------------------------------------------------------------------------------
-- | Workspace configuration and utilities.
module XMonad.Local.Workspaces
       ( names
       , hidden
       , asKey
       , viewPrevWS
       ) where

--------------------------------------------------------------------------------
import Control.Monad (when)
import XMonad
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- | Names of my workspaces. I have two sets of workspaces:
--
--  * Primary: 1 through 9 and 0 (i.e. the number row on a keyboard)
--  * Secondary: F1 through F12
names :: [String]
names = map show primary ++ map (("F" ++) . show) secondary
  where primary   = [1..9] ++ [0] :: [Int]
        secondary = [1..12]       :: [Int]

--------------------------------------------------------------------------------
-- | Names of workspaces that should be hidden from display when they
-- have no windows in them.
hidden :: [String]
hidden = filter (\(x:_) -> x == 'F') names

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
  when (not $ null hs) (windows . W.view . W.tag $ head hs)
