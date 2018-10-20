{-# LANGUAGE EmptyDataDecls #-}

-- | Represent a list that has been sorted on a particular feature.
--
--   It is an experiment, and is probably both over-engineered *and*
--   under-powered for what I want.
--
module Sorted ( SortedList
              , emptySL
              , toSL
              , unsafeToSL
              , fromSL
              , mergeSL
              , lengthSL
              , nullSL

                -- These are used by various parts of the system; at some
                -- level having them here is a bit out of place, but at
                -- least if you need one of them you will also need
                -- some other symbol from this module.
                --
              , StartTimeOrder
              , ExposureTimeOrder
                
             ) where

import Data.Function (on)
import Data.List (sortBy)

-- | A list of items, in ascending order, for a given view of
--   the data (using a phantom type for this evidence).
--
--   I think that really the projection function should be
--   carried along somehow, since this is needed to really
--   make the Monoid instance useful, but not sure how
--   to do this. Perhaps I should look at functional dependencies,
--   but I haven't really scoped out what information I
--   want to carry along (to see what I really need).
--   For the current use case I do not think that this
--   abstraction is actually useful, but leave as is for now.
--
newtype SortedList f a = SL { _unSL :: [a] }

{- want something like the following, but ideally without
   having to carry the projection function around, so that
   empty instances can be created
data SortedList2 a b = SL2 { _slProj :: a -> b
                           , _slList :: [a] }
-}

{-
instance Eq a => Eq (SortedList f a) where
  (==) = (==) `on` _unSL
-}

instance Functor (SortedList f) where
  fmap f (SL a) = SL (fmap f a)

{-
instance Ord a => Monoid (SortedList f a) where
  mempty = emptySL
  mappend = mergeSL
-}

-- | The empty sorted list.
emptySL :: SortedList f a
emptySL = SL []

lengthSL :: SortedList f a -> Int
lengthSL (SL xs) = length xs

-- | The input list *must* be sorted in ascending order, but
--   it is not checked.
unsafeToSL :: [a] -> SortedList f a
unsafeToSL = SL

-- | The input list need not be in ascending order.
toSL ::
  Ord b
  => (a -> b)  -- ^ projection function to get the item to sort on
  -> [a]
  -> SortedList f a
toSL p = SL . sortBy (compare `on` p)

-- | The list remains sorted (in ascending order).
fromSL :: SortedList f a -> [a]
fromSL = _unSL

-- | Is the list empty?
nullSL :: SortedList f a -> Bool
nullSL = null . _unSL

-- | Merge two sorted lists.
--
--   It is assumed that b and f are related, in that
--   sorting on b retains the order given by f.
--
mergeSL ::
  Ord b
  => (a -> b)  -- ^ projection function
  -> SortedList f a
  -> SortedList f a
  -> SortedList f a
mergeSL _ x@(SL _) (SL []) = x
mergeSL _ (SL []) y@(SL _) = y
mergeSL p (SL xs) (SL ys) = SL (go xs ys)
  where
    go x0 [] = x0
    go [] y0 = y0
    go x0@(x:x1) y0@(y:y1) | p x > p y = y : go x0 y1
                           | otherwise = x : go x1 y0


-- | Indicate that a list is sorted by start time (earliest first)
data StartTimeOrder

-- | Indicate that a list is sorted by exposure time (shortest first)
data ExposureTimeOrder

