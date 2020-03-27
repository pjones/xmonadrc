{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Common layout utilities.
module XMonad.Local.Layout.Common
  ( Distribution(..)
  , distSucc
  , distPred
  , distribute
  , swapSpacesByIndex
  , spaceContainingWindow
  , splitVerticallyFrac
  , splitHorizontallyFrac
  , RectangleF(..)
  ) where

--------------------------------------------------------------------------------
-- Imports:
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Foldable
import Data.Monoid
import Graphics.X11 (Rectangle(..))
import XMonad.Layout (mirrorRect)

--------------------------------------------------------------------------------
data Distribution
  = Exactly Int
  | NoMoreThan Int
  | Balance
  deriving (Show, Read, Eq)

instance Semigroup Distribution where
  (<>) Balance x = x
  (<>) x Balance = x
  (<>) (NoMoreThan x) (NoMoreThan y) = NoMoreThan (x + y)
  (<>) (Exactly x) (Exactly y) = Exactly (x + y)
  (<>) (Exactly x) _ = Exactly x
  (<>) _ (Exactly x) = Exactly x

instance Monoid Distribution where
  mempty = Balance

--------------------------------------------------------------------------------
-- | Add an additional window to a 'Distribution'.
distSucc :: Distribution -> Distribution
distSucc = \case
  Balance      -> NoMoreThan 1
  NoMoreThan n -> NoMoreThan (succ n)
  Exactly n    -> Exactly (succ n)

--------------------------------------------------------------------------------
-- | Decrease the number of windows.
distPred :: Distribution -> Distribution
distPred = \case
  Balance      -> Exactly 1
  NoMoreThan 1 -> Balance
  NoMoreThan n -> NoMoreThan (pred n)
  Exactly 1    -> Balance
  Exactly n    -> Exactly (pred n)

--------------------------------------------------------------------------------
-- | Distribute windows among a traversable structure.
--
-- The structure is annotated with the number of windows that each
-- space should consume.  If an annotation is 'Nothing' that space was
-- not used and will consume zero windows.
distribute
  :: forall t a. Traversable t
  => Int                    -- ^ Number of windows to distribute.
  -> (a -> Distribution)    -- ^ Extract the distribution method.
  -> t a                    -- ^ The structure.
  -> t (a, Maybe Int)       -- ^ Structure with window count.
distribute windows extract struct =
    evalState (traverse go0 struct) (windows, dists)

  where
    -- List of distribution methods.
    dists :: [Distribution]
    dists = foldMap ((:[]) . extract) struct

    -- Distribute windows using a state monad.  The state we are
    -- tracking is the number of remaining windows and the remaining
    -- ways we are to distribute windows.
    go0 :: a -> State (Int, [Distribution]) (a, Maybe Int)
    go0 slot = do
      (remaining, ds) <- get
      if remaining < 1
        then pure (slot, Nothing)
        else (slot,) <$> go1 remaining ds

    -- We know the remaining number of windows is positive.
    go1 :: Int -> [Distribution] -> State (Int, [Distribution]) (Maybe Int)
    go1 r [ ]    = takeAllWindows r
    go1 r [_]    = takeAllWindows r
    go1 r (d:ds) = case d of
      Exactly n ->
        takeWindows n r
      NoMoreThan n ->
        let toTake = min n (lookahead r (d:ds))
        in takeWindows toTake r
      Balance ->
        takeWindows (lookahead r (d:ds)) r

    -- Guesstimate how many windows should be taken given the
    -- remaining distribution methods.
    lookahead :: Int -> [Distribution] -> Int
    lookahead 0 _  = 0
    lookahead _ [] = 0
    lookahead r ds =
      let fixed = getSum $ foldMap Sum [ e | Exactly e <- ds ]
          others = length (filter balanced ds)
          todist = subtract fixed r
      in if todist < 1 || others < 1
           then r
           else ceiling (fromIntegral todist / fromIntegral others :: Double)

    balanced :: Distribution -> Bool
    balanced = \case
      Exactly _    -> False
      NoMoreThan _ -> True
      Balance      -> True

    -- Take /N/ windows from the pool and calculate how many are left.
    takeWindows
      :: Int -- ^ Number of windows to take.
      -> Int -- ^ Remaining windows available.
      -> State (Int, [Distribution]) (Maybe Int)
    takeWindows n r =
      if r >= n
        then update (subtract n r) >> pure (Just n)
        else update 0              >> pure (Just r)

    -- Take all remaining windows.
    takeAllWindows :: Int -> State (Int, [Distribution]) (Maybe Int)
    takeAllWindows r | r < 1     = pure Nothing
                     | otherwise = takeWindows r r

    -- | Update the state recording the number of remaining windows
    -- and removing the head of the remaining distribution methods.
    update :: Int -> State (Int, [Distribution]) ()
    update n = modify' (bimap (const n) (drop 1))

--------------------------------------------------------------------------------
swapSpacesByIndex :: forall t a. Traversable t => Int -> Int -> t a -> t a
swapSpacesByIndex i j s =
    evalState (traverse rebuild s) 0
  where
    asList :: [a]
    asList = toList s

    swapElems :: a -> Int -> a
    swapElems x n
      | n == i    = asList !! j
      | n == j    = asList !! i
      | otherwise = x

    rebuild :: a -> State Int a
    rebuild x = swapElems x <$> get <* modify' succ

--------------------------------------------------------------------------------
-- | Tag the space containing a window with the given index.
spaceContainingWindow
  :: forall t a. Traversable t
  => Int              -- ^ Index of the window you are looking for.
  -> (a -> Maybe Int) -- ^ Get the number of windows in this space.
  -> t a              -- ^ The structure containing windows.
  -> t (a, Maybe Int) -- ^ The structure tagged to indicate if it
                      --   contains the window along with its index in
                      --   the space.
spaceContainingWindow ix extract structure =
    evalState (traverse go structure) 0
  where
    -- State is the number of windows we've seen so far.
    --
    -- Note: `ix' is a zero-based index
    --       `before' is a one-based count.
    go :: a -> State Int (a, Maybe Int)
    go x = do
      before <- get
      if ix < before then pure (x, Nothing)
      else case extract x of
        Nothing -> pure (x, Nothing)
        Just count -> do
          let after = before + count
          put after
          if ix < after
            then pure (x, Just (ix - before))
            else pure (x, Nothing)

--------------------------------------------------------------------------------
-- | Given a list of fractional sizes and a rectangle, split that
-- rectangle vertically into subrectangles.
splitVerticallyFrac
  :: forall a. RealFrac a
  => [a]
  -> Rectangle
  -> [Rectangle]
splitVerticallyFrac = go where
  go :: [a] -> Rectangle -> [Rectangle]
  go []  r = [r]
  go [_] r = [r]
  go (f:fs) (Rectangle sx sy sw sh) =
    let r1 = Rectangle sx sy sw mh
        r2 = Rectangle sx my sw (sh - mh)
        mh = min sh (floor $ fromIntegral (sh `div` ln) * f)
        my = sy + fromIntegral mh
        ln = fromIntegral (length fs + 1)
    in r1 : go fs r2

--------------------------------------------------------------------------------
-- | Given a list of fractional sizes and a rectangle, split that
-- rectangle horizontally into subrectangles.
splitHorizontallyFrac :: RealFrac a => [a] -> Rectangle -> [Rectangle]
splitHorizontallyFrac fs = fmap mirrorRect . splitVerticallyFrac fs . mirrorRect

--------------------------------------------------------------------------------
newtype RectangleF = RectangleF
  { getRect :: Rectangle
  }
  deriving (Eq, Show, Read)

instance Semigroup RectangleF where
  (<>) (RectangleF x) (RectangleF y) = RectangleF $
    Rectangle (min (rect_x x)      (rect_x y))
              (min (rect_y x)      (rect_y y))
              (max (rect_width x)  (rect_width y))
              (max (rect_height x) (rect_height y))

instance Monoid RectangleF where
  mempty = RectangleF $ Rectangle 0 0 0 0
