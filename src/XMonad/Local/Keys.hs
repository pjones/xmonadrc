--------------------------------------------------------------------------------
-- | Key bindings.
module XMonad.Local.Keys (keys) where

--------------------------------------------------------------------------------
import qualified Data.Map as M
import Graphics.X11.Xlib (KeyMask, KeySym)
import XMonad.Core (X, XConfig, Layout)
-- import Graphics.X11.Xlib.Extras

--------------------------------------------------------------------------------
keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys = undefined
