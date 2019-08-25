--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Color theme.
module XMonad.Local.Theme
       ( xmonadColors
       ) where

--------------------------------------------------------------------------------
import XMonad

--------------------------------------------------------------------------------
xmonadColors :: XConfig a -> XConfig a
xmonadColors x = x { normalBorderColor  = "#444444"
                   , focusedBorderColor = "#00e8c6"
                   , borderWidth        = 2
                   }
