--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Check parts of my XMonad configuration, like key bindings.
module Main where

--------------------------------------------------------------------------------
import Control.Monad (void, unless)
import qualified Data.Map as M
import qualified Data.Set as S
import Graphics.X11
import Graphics.X11.Xinerama
import Graphics.X11.Xrandr
import XMonad
import XMonad.Prompt
import XMonad.StackSet (new)
import XMonad.Util.EZConfig (checkKeymap)
import XMonad.Util.Font

--------------------------------------------------------------------------------
import XMonad.Local.Keys (rawKeys)
import XMonad.Local.Prompt (promptConfig)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  dpy    <- openDisplay ""
  rootw  <- rootWindow dpy $ defaultScreen dpy

  let xmc = def {layoutHook = Layout $ layoutHook def, modMask = mod3Mask}
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

  void $ runX cf st $ do
    -- Check key bindings, errors go to xmessage.
    checkKeymap xmc (rawKeys xmc)

    -- Make sure font rendering works.
    xmf <- initXMF (font promptConfig)

    case xmf of
      Core _ -> io (putStrLn "Font: core")
      Utf8 _ -> io (putStrLn "Font: utf8")
      Xft  _ -> io (putStrLn "Font: xft")

    -- Test for XRandR support.
    unless compiledWithXrandr (error "no XRandR support!")
    io $ putStrLn ("Screens: " ++ show (screenCount dpy))
    io (getScreenInfo dpy >>= putStrLn . show)
