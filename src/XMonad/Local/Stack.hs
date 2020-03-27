{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Additional features for stacks.
module XMonad.Local.Stack
  ( refocus
  , focusIndex
  , overFocus
  , toStack
  , stackSize
  , zipStacks
  , zipStacksWith
  , cullStack
  , TraverseStyle(..)
  , StackF(..)
  , stackF
  , setTraverseStyle
  ) where

--------------------------------------------------------------------------------
import Data.Maybe
import XMonad.StackSet (Stack(Stack))
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- | Refocus a stack on the given element.
refocus :: forall a. Monoid a => Int -> Stack a -> Stack a
refocus focus stack =
    go 0 [] (W.integrate stack)
  where
    go :: Int -> [a] -> [a] -> Stack a
    go _ up [] = Stack mempty up []
    go n up (x:down) | n == focus = Stack x up down
                     | otherwise  = go (n+1) (x:up) down

--------------------------------------------------------------------------------
-- | Build a stack from a list where the /Nth/ element has the focus.
toStack :: forall a. Monoid a => Int -> [a] -> Stack a
toStack _    [ ] = Stack mempty [] []
toStack _    [x] = Stack x [] []
toStack focus xs = case splitAt focus xs of
  ([], [])     -> Stack mempty [] []
  (up, f:down) -> Stack f (reverse up) down
  (f:up, down) -> Stack f (reverse up) down

--------------------------------------------------------------------------------
-- | The index of the focused element.
focusIndex :: Stack a -> Int
focusIndex = length . W.up

--------------------------------------------------------------------------------
stackSize :: Stack a -> Int
stackSize = length . W.integrate

--------------------------------------------------------------------------------
overFocus :: (a -> a) -> Stack a -> Stack a
overFocus f s = s { W.focus = f (W.focus s) }

--------------------------------------------------------------------------------
-- | See 'zipStacksWith'.
zipStacks :: (Monoid a, Monoid b) => Stack a -> Stack b -> Stack (a, b)
zipStacks = zipStacksWith (,)

--------------------------------------------------------------------------------
-- | Zip two stacks.  The resulting stack will have it's focus set to
-- the focus of the first stack.
zipStacksWith :: Monoid c => (a -> b -> c) -> Stack a -> Stack b -> Stack c
zipStacksWith f x y =
  toStack (focusIndex x) (zipWith f (W.integrate x) (W.integrate y))

--------------------------------------------------------------------------------
cullStack :: Monoid b => (a -> Maybe b) -> Stack a -> Stack b
cullStack f s = Stack (fromMaybe mempty (f $ W.focus s))
                      (mapMaybe f $ W.up s)
                      (mapMaybe f $ W.down s)

--------------------------------------------------------------------------------
data TraverseStyle
  = TraverseAsList
  | TraverseFocusUpDown
  | TraverseFocusDownUp
  deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- | A newtype wrapper around 'Stack' to provide useful instances.
data StackF a = StackF
  { getStack :: Stack a
  , traverseStyle :: TraverseStyle
  }
  deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
stackF :: Stack a -> StackF a
stackF = (`StackF` TraverseAsList)

--------------------------------------------------------------------------------
setTraverseStyle :: TraverseStyle -> StackF a -> StackF a
setTraverseStyle t s = s { traverseStyle = t }

-- instance Monoid a => Monoid (StackF a) where
--   mempty = StackF (Stack mempty [] []) TraverseAsList

instance Functor StackF where
  fmap f (StackF s t) = StackF
    { getStack =
        Stack (f  $  W.focus s)
              (f <$> W.up    s)
              (f <$> W.down  s)
    , traverseStyle = t
    }

instance Foldable StackF where
  foldMap f = foldMap f . toList
    where
      toList :: StackF a -> [a]
      toList (StackF s t) = case t of
        TraverseAsList ->
          W.integrate s
        TraverseFocusUpDown ->
          W.focus s : W.up s <> W.down s
        TraverseFocusDownUp ->
          W.focus s : W.down s <> W.up s

instance Traversable StackF where
  traverse f (StackF s t) =
      StackF
        <$> go t
        <*> pure t
    where
      up    = reverse <$> traverse f (reverse $ W.up s)
      down  = traverse f (W.down s)
      focus = f (W.focus s)

      go = \case
        TraverseAsList ->
          flip Stack <$> up <*> focus <*> down
        TraverseFocusUpDown ->
          Stack <$> focus <*> up <*> down
        TraverseFocusDownUp ->
          (\f' d u -> Stack f' u d) <$> focus <*> down <*> up
