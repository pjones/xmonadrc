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
-- Color theme.
module XMonad.Local.Theme
  ( xmonadColors,
  )
where

import XMonad

xmonadColors :: XConfig a -> XConfig a
xmonadColors x =
  x
    { normalBorderColor = "#000000",
      focusedBorderColor = "#00e8c6",
      borderWidth = 4
    }
