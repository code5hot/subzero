{-
SubZero, A haskell library to provide a useful data structure
Copyright (C) 2017 Tristan Wibberley

This program is free software; you can redistribute it and/or modify
it under the terms of version 2 of the GNU General Public License
as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

module Control.Applicative.SubZero
    ( SubZero
    , foldlA1
    , letStand
    , points
    , fromSubZero
    , collapse
    ) where

import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.Function

-- SubZero
-- f = major functor
-- g = superposition policy - how do they collapse?
-- a = transformed value type
newtype SubZero f g a = SubZero { getSubZero :: f (g a) }
instance (Functor f, Functor g) => Functor (SubZero f g) where
  fmap f s = SubZero $ f' <$> a
    where a = getSubZero s
          f' = fmap f

instance (Applicative f, Applicative g) => Applicative (SubZero f g) where
  pure v = (SubZero . pure . pure) v
  r <*> s = SubZero $ f' <*> b
    where a = getSubZero r
          b = getSubZero s
          f' = fmap (<*>) a

instance (Applicative f, Alternative g) => Alternative (SubZero f g) where
  empty   = SubZero $ pure empty
  r <|> s = SubZero $ f' <*> b
    where a = getSubZero r
          b = getSubZero s
          f' = fmap (<|>) a

-- A foldl for foldable alternatives, empty stays empty, nonempty becomes one alternative
-- The applicative you get back can be a different type than the input
foldlA1 :: (Foldable f, Alternative g) => (a -> a -> a) -> f a -> g a
foldlA1 f a | null a    = empty
            | otherwise = pure $ foldl1 f a

--    Turns a value to Just value or Nothing based on a predicate assuming you use it in a
-- context that wants Maybe instead of some other representation of Alternatives
letStand :: (Alternative f) => (a -> Bool) -> a -> f a
letStand f x | f x       = pure x
             | otherwise = empty

--    Turns a container of values to a container of either
-- still-standing or destroyed values based on a predicate
--    The type constraint allows us to model possible outcomes
-- where destroyed values are just "no possible outcomes"
points :: (Functor f, Alternative g) => (a -> Bool) -> f a -> SubZero f g a
points f = SubZero . ((letStand f) <$>)

--    If the type of the possibilities concept is Maybe then you can
-- use fromSubZero with a default for impossible points
fromSubZero :: (Applicative f) => f a -> SubZero f Maybe a -> f a
fromSubZero a s = fromMaybe <$> a <*> (getSubZero s)

--     Take the alternatives embedded in the SubZero and collapse them
-- with a collapse function to a single alternative or empty (no possible outcomes)
--     This is quite free in the type of the possibilities concept of the result
--     It's compatible with Maybe for further uses with fromSubZero, but you can
-- retain behaviours of more sophisticated types. A consequence of this
-- is that you will probably require to state a type
collapse :: (Functor f, Foldable g, Alternative h) => (a -> a -> a) -> SubZero f g a -> SubZero f h a
collapse f s = SubZero $ f' <$> a
  where a = getSubZero s
        f' = foldlA1 f

