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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
    Module      : Control.Applicative.SubZero
    Description : SubZero type and related functions
    Copyright   : (c) Tristan Wibberley, 2017
    License     : GPL-2
    Maintainer  : tristan.wibberley@gmail.com
    Stability   : experimental

    Here is a longer description of this module, containing some
    commentary with @some markup@.
-}
module Control.Applicative.SubZero
    ( SubZero
      -- * Constructors and Translators
      -- 
      -- $constructors
    , points
    , foldlA1
    , letStand
    , fromSubZero
    , collapse
    ) where

import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.Function
import Data.Functor.Compose

{- |
   Converts a functor so that each point at the source has alternatives.

   It's just like Compose but the applicative instance appends new
   alternative values in the rightmost (inner/minor) functor instead of
   in the leftmost (outer/major) functor.

   The result is that two ZipLists of alternatives zip together,
   providing alternatives to each point and two

   Given the immediate utility of this, I do wonder if the Alternative
   instance of Compose is the wrong one.

  f = major functor
  g = superposition policy - how do they collapse?
  a = transformed value type
-}
newtype SubZero f g a =
  SubZero { getSubZero :: Compose f g a }
  deriving (Show, Read, Eq, Ord, Functor, Applicative)

instance (Applicative f, Alternative g) => Alternative (SubZero f g) where
  empty   = SubZero $ Compose $ pure empty
  (SubZero (Compose a)) <|> (SubZero (Compose b)) = SubZero $ Compose $ (<|>) <$> a <*> b

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

{- $constructors

   Constructors are good
-}

{- | Turns a container of values to a container of either
   retained or destroyed values based on a predicate

     The type constraint allows us to model possible outcomes
   so destroyed values are just "no possible outcomes" while
   retained values represent "the only possible outcome".

     To represent that "no value" is a possible outcome, @a@
   should be some type like ('Maybe' a) or ('Either' 'String' a).

   [@f@]: This functor defines the broad scale
          behaviours but its Alternative instance is overridden.
          This in particular might change during upcoming design
          validation.
   [@g@]: This functor supplies the supercedent Alternative
          instance and thus the finer behaviours.
-}
points :: 
  (Functor f, Alternative g)
    => (a -> Bool)   -- ^ A predicate that indicates whether a point
                     -- is occupied by its original value or vacant.
    -> f a           -- ^ The seed points with their values.
    -> SubZero f g a -- ^ The constructed @SubZero@ value.
points f = SubZero . Compose . ((letStand f) <$>)


--    If the type of the possibilities concept is Maybe then you can
-- use fromSubZero with a default for impossible points
fromSubZero :: (Applicative f) => f a -> SubZero f Maybe a -> f a
fromSubZero a (SubZero (Compose b)) = fromMaybe <$> a <*> b

--     Take the alternatives embedded in the SubZero and collapse them
-- with a collapse function to a single alternative or empty (no possible outcomes)
--     This is quite free in the type of the possibilities concept of the result
--     It's compatible with Maybe for further uses with fromSubZero, but you can
-- retain behaviours of more sophisticated types. A consequence of this
-- is that you will probably require to state a type
collapse :: (Functor f, Foldable g, Alternative h) => (a -> a -> a) -> SubZero f g a -> SubZero f h a
collapse f (SubZero (Compose a)) = SubZero $ Compose $ (foldlA1 f) <$> a

