{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module XMonad.Local.Layout.Columns
  ( Columns
  , IncMasterCol(..)
  , TileFlow(..)
  , IncLayoutN(..)
  , mkCols
  ) where

--------------------------------------------------------------------------------
import Control.Monad
import Data.Bifunctor
import Data.Foldable
import Data.Function
import Data.List (findIndex)
import Data.Maybe
import Graphics.X11 (Rectangle(..), Position)
import XMonad.Core
import XMonad.Layout.LayoutBuilder (IncLayoutN(..))
import XMonad.Layout.ResizableTile (MirrorResize(..))
import XMonad.Local.Layout.Common
import XMonad.Local.Stack
import XMonad.StackSet (Stack(Stack))
import qualified XMonad.StackSet as W

import XMonad.Layout
  ( Resize(..)
  , IncMasterN(..)
  , splitHorizontallyBy
  , splitHorizontally
  )

--------------------------------------------------------------------------------
-- | A message to control the column where the master window is in.
newtype IncMasterCol = IncMasterCol Int
  deriving Typeable

instance Message IncMasterCol

--------------------------------------------------------------------------------
data TileFlow
  = RightToLeft -- ^ Tile windows to the right of the master column first.
  | LeftToRight -- ^ Tile windows to the left of the master column first.
  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
toTraverseStyle :: TileFlow -> TraverseStyle
toTraverseStyle = \case
  RightToLeft -> TraverseFocusDownUp
  LeftToRight -> TraverseFocusUpDown

--------------------------------------------------------------------------------
data Col = Col
  { colDist    :: !Distribution -- ^ How windows are distributed.
  , colHeights :: [Rational]    -- ^ Window height offsets.
  , colWins    :: !Int          -- ^ Number of windows in this column.
  }
  deriving (Eq, Show, Read)

instance Semigroup Col where
  (<>) x y = Col (colDist x    <> colDist y)
                 (colHeights x <> colHeights y)
                 (colWins x    +  colWins y)

instance Monoid Col where
  mempty = Col mempty mempty 0

--------------------------------------------------------------------------------
data Columns a = Columns
  { masterColumnNumber :: !Int
    -- ^ Which column the master window should be in.

  , maxColumns :: !Int
    -- ^ Don't use more than this many columns.

  , changeSizeDelta :: !Rational
    -- ^ The amount to change the width of a column or height of a
    -- window when resizing.

  , columnStack :: !(Stack Col)
    -- ^ Stack of columns where the focused element represents the
    -- column with the current focus.

  , masterColumnSize :: !Rational
    -- ^ Relative size of the master column.

  , tileFlow :: !TileFlow
    -- ^ How to tile windows relative to the master column.

  , focusedWindowIndex :: !Int
    -- ^ The index of the focused window relative to the other windows
    -- in the same column.
  }
  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- | Create a column layout.
--
-- NOTE: Column numbers start at 0.
mkCols
  :: Int       -- ^ Which column to put the master window into.
  -> Int       -- ^ Initial number of master windows.
  -> Int       -- ^ Maximum number of columns (to start with).
  -> Rational  -- ^ Ratio of screen space for the master column.
  -> Rational  -- ^ Amount to grow/shrink columns and windows.
  -> TileFlow  -- ^ Order in which to lay out windows.
  -> Columns a -- ^ Columns layout.
mkCols mcol' mwins ncols' frac delta flow =
  let ncols = max 1 ncols'
      mcol  = min ncols $ max 0 mcol'
      stack = Stack (Col (Exactly mwins) mempty 0)
                (replicate mcol mempty)
                (replicate (ncols - mcol - 1) mempty)
  in Columns
      { masterColumnNumber = mcol
      , masterColumnSize   = frac
      , maxColumns         = ncols
      , changeSizeDelta    = delta
      , tileFlow           = flow
      , columnStack        = stack
      , focusedWindowIndex = 0
      }

--------------------------------------------------------------------------------
instance LayoutClass Columns a where
  doLayout :: Columns a -> Rectangle -> Stack a -> X ([(a, Rectangle)], Maybe (Columns a))
  doLayout self rect stack =
    let windows = W.integrate stack
        (rectangles, layout) = tile self (focusIndex stack) rect (length windows)
    in pure (zip windows rectangles, if self == layout then Nothing else Just layout)

  pureMessage self m = msum
    [ fmap (`changeColumnWindowCount` self) (fromMessage m)
    , fmap (`changeMasterColumnWidth` self) (fromMessage m)
    , fmap (`changeNumberOfColumns`   self) (fromMessage m)
    , fmap (`changeMasterColumn`      self) (fromMessage m)
    , fmap (`changeWindowHeight`      self) (fromMessage m)
    ]

  description _ = "Columns"

--------------------------------------------------------------------------------
-- | Adjust the number of windows allowed in the focused column.
changeColumnWindowCount :: IncMasterN -> Columns a -> Columns a
changeColumnWindowCount (IncMasterN n) layout@Columns{..}
    | n >= 0    = go (modC distSucc)
    | otherwise = go (modC distPred)
  where
    go f = layout
      { columnStack =
          overFocus (fold . (replicate (abs n) f <*>) . pure) columnStack
      }

    -- I should just bring in lenses.
    modC f c = c { colDist = f (colDist c) }

--------------------------------------------------------------------------------
-- | Adjust the size of the master column.
changeMasterColumnWidth :: Resize -> Columns a -> Columns a
changeMasterColumnWidth = \case
  Shrink -> \c@Columns{..} ->
    c { masterColumnSize = max 0 (masterColumnSize - changeSizeDelta) }
  Expand -> \c@Columns{..} ->
    c { masterColumnSize = min 1 (masterColumnSize + changeSizeDelta) }

--------------------------------------------------------------------------------
-- Change the maximum number of columns.
changeNumberOfColumns :: IncLayoutN -> Columns a -> Columns a
changeNumberOfColumns (IncLayoutN n) layout =
  layout
    { maxColumns  = newCount
    , columnStack = newStack
    }
  where
    newCount = max (masterColumnNumber layout + 1) (maxColumns layout + n)

    newStack =
      let mcol = masterColumnNumber layout
          s    = refocus mcol (columnStack layout)
      in Stack (W.focus s)
          (take mcol $ W.up s ++ repeat mempty)
          (take (newCount - mcol - 1) $ W.down s ++ repeat mempty)

--------------------------------------------------------------------------------
-- | Adjust which column contains the master window.
changeMasterColumn :: IncMasterCol -> Columns a -> Columns b
changeMasterColumn (IncMasterCol n) layout@Columns{..} =
  layout
    { masterColumnNumber = newCol
    , columnStack        = newStack
    }

  where
    newCol = max 0 $ min (stackSize columnStack - 1) (masterColumnNumber + n)
    newStack = stackF columnStack
             & swapSpacesByIndex masterColumnNumber newCol
             & getStack

--------------------------------------------------------------------------------
-- | Adjust the height of the focused window.
changeWindowHeight :: MirrorResize -> Columns a -> Columns a
changeWindowHeight resize layout@Columns{..} =
  let f = case resize of
            MirrorShrink -> subtract changeSizeDelta
            MirrorExpand -> (+ changeSizeDelta)
  in layout
        { columnStack =
            overFocus (updateCol f) columnStack
        }
  where
    updateCol :: (Rational -> Rational) -> Col -> Col
    updateCol f c = c { colHeights = update (ix c) f (heights c) }

    ix :: Col -> Int
    ix (Col _ _ n)
      | focusedWindowIndex == (n - 1) = pred focusedWindowIndex
      | otherwise                     = focusedWindowIndex

    update :: Int -> (Rational -> Rational) -> [Rational] -> [Rational]
    update _ _ [] = []
    update 0 f (r:rs) = f r : rs
    update n f (r:rs) = r : update (n-1) f rs

    heights :: Col -> [Rational]
    heights (Col _ hs n) = take n (hs ++ repeat 1)

--------------------------------------------------------------------------------
tile :: Columns a -> Int -> Rectangle -> Int -> ([Rectangle], Columns a)
tile layout@Columns{..} focus rect windows =
    ( rectangles
    , layout { columnStack        = finalStack
             , focusedWindowIndex = winIndex
             }
    )
  where
    rectangles :: [Rectangle]
    rectangles = toColumns mastered
               & concatMap toRows

    (colIndex, winIndex) = getWindowColumnAndIndex mastered focus

    finalStack :: Stack Col
    finalStack = mastered
               & getStack
               & W.integrate
               & toStack colIndex
               & stackF
               & getStack

    -- Stack expanded to maximum columns with the master column focused.
    mastered :: StackF Col
    mastered = W.integrate columnStack
             & \cs -> (length cs, cs)
             & first (min masterColumnNumber)
             & uncurry toStack
             & stackF
             & setTraverseStyle (toTraverseStyle tileFlow)
             & distribute windows colDist
             & fmap (\(c, n) -> c { colWins = fromMaybe 0  n })

    toColumns :: StackF Col -> [(Col, RectangleF)]
    toColumns s = toList rects
      where
        rects :: StackF (Col, RectangleF)
        rects = splitColumns cs masterColumnSize rect
              & zipStacks cs
              & stackF
              & setTraverseStyle (toTraverseStyle tileFlow)
          where cs = alive (getStack s)

        alive :: Stack Col -> Stack Col
        alive = let intToMaybe n = if n < 1 then Nothing else Just n
                in cullStack (\c -> c <$ intToMaybe (colWins c))

    toRows :: (Col, RectangleF) -> [Rectangle]
    toRows (c, RectangleF r) =
      let fracs = take (colWins c) (colHeights c ++ repeat 1)
      in splitVerticallyFrac fracs r

--------------------------------------------------------------------------------
-- | Find the column number that a window is in.  The stack must be
-- focused on the master column.
getWindowColumnAndIndex :: StackF Col -> Int -> (Int, Int)
getWindowColumnAndIndex cs ix =
    let cix = fromMaybe 0 $ findIndex isJust tagged
        wix = fromMaybe 0 $ msum tagged
    in (cix, wix)
  where
    tagged :: [Maybe Int]
    tagged = spaceContainingWindow ix (Just . colWins) cs
           & getStack
           & W.integrate
           & fmap snd

--------------------------------------------------------------------------------
-- | Split a rectangle into columns.
splitColumns
  :: Stack a          -- ^ Stack holding columns.
  -> Rational         -- ^ How big is the master column?
  -> Rectangle        -- ^ Space to fit the columns into.
  -> Stack RectangleF -- ^ Columns.
splitColumns s frac rect
    | ncols < 2 = Stack (RectangleF rect) [] []
    | mcol == 0 = toStack mcol (fmap RectangleF masterFirst)
    | otherwise = toStack mcol (fmap RectangleF (moveMaster masterFirst))

  where
    ncols :: Int
    ncols = stackSize s

    mcol :: Int
    mcol = focusIndex s

    masterFirst :: [Rectangle]
    masterFirst =
      let (master, rest) = splitHorizontallyBy frac rect
      in master : splitHorizontally (pred ncols) rest

    moveMaster :: [Rectangle] -> [Rectangle]
    moveMaster []     = [rect] -- Should be impossible.
    moveMaster [r]    = [r]    -- Same.
    moveMaster (r:rs) = fixX (rect_x rect) (take mcol rs ++ [r] ++ drop mcol rs)

    fixX :: Position -> [Rectangle] -> [Rectangle]
    fixX _ []     = []
    fixX x (r:rs) = r {rect_x = x} : fixX (x + fromIntegral (rect_width r)) rs
