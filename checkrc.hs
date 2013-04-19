--------------------------------------------------------------------------------
-- | Check parts of my XMonad configuration, like key bindings.
module Main where

--------------------------------------------------------------------------------
import qualified Data.Map as M
import qualified Data.Set as S
import XMonad
import XMonad.Local.Keys (rawKeys)
import XMonad.StackSet (new)
import XMonad.Util.EZConfig (checkKeymap)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  dpy    <- openDisplay ""
  rootw  <- rootWindow dpy $ defaultScreen dpy

  let initcf = defaultConfig
      xmc = initcf {layoutHook = Layout $ layoutHook initcf}
      initialWinset = new (layoutHook xmc) (workspaces xmc) []

  let cf = XConf { display       = dpy
                 , config        = xmc
                 , theRoot       = rootw
                 , normalBorder  = 0
                 , focusedBorder = 0
                 , keyActions    = M.empty
                 , buttonActions = M.empty
                 , mouseFocused  = False
                 , mousePosition = Nothing
                 , currentEvent  = Nothing
                 }

  let st = XState { windowset       = initialWinset
                  , numberlockMask  = 0
                  , mapped          = S.empty
                  , waitingUnmap    = M.empty
                  , dragging        = Nothing
                  , extensibleState = M.empty
                  }

  -- Check key bindings, errors go to xmessage.
  _ <- runX cf st $ checkKeymap xmc (rawKeys xmc)
  return ()
