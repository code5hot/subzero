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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
    Module      : Control.Applicative.SubZero
    Description : SubZero type and related functions
    Copyright   : (c) Tristan Wibberley, 2017
    License     : GPL-2
    Maintainer  : tristan.wibberley@gmail.com
    Stability   : experimental

   Converts a functor so that each point at the source has alternatives
   to model things like a container of optional values, or of a variety
   of opinions about a point.

   It turns out that 'Data.Functor.Compose.Compose' is ideal for this
   because @'Functor' f => f a@ is isomorphic to @'Functor' f =>
   'Data.Functor.Compose.Compose' f 'Data.Functor.Identity.Identity' a@.

   The gorgeous result is that two @'ZipList'@s of alternatives can zip
   together, providing an expanding set of alternatives to each point.

   Here's how you can interpret @'Data.Functor.Compose.Compose' f g a@:

   [@f@]: The major functor, overall mapping/view
   [@g@]: This has a few key useful interpretations depending on
       its instances, examples below.
   [@a@]: Transformed/contained value type.

   Some example instances that you might want to rely on from @g@:

   [@'Alternative'@]: Superposition functor.

       - How do individual items have a set of
       possible values?
       - How do those possible values collapse to
       form one optional value?
       - etc.

   [etc]: There are a lot of other utilities for this type.
-}
module Control.Applicative.SubZero
    ( -- * Constructors
      --  $constructors
      points
    , reveal
      -- * Destructors
      --  $destructors
    , flatten
      -- * Restructors
      --  $restructors
    , (<-$>)
    , universal
    , (<-|>)
    , Superposition
    , simplify
    , collapse
      -- * Instructors
      --  $instructors
    , keep
    , keepIdentity
    ) where

import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.Function
import Data.Functor.Compose
import Data.Functor.Identity

{-  $constructors
-}

-- | Provides structure for values at the other end of a @'Functor'@
reveal
  :: (Functor f, Applicative g)
    => f a           -- ^ Initial flat structure
    -> Compose f g a -- ^ enhanced structure, albeit with no changes
reveal = Compose . (pure <$>)

{- | Turns a container of values to a container of either
   retained or destroyed values based on a predicate

     The type constraint allows us to model possible outcomes
   so destroyed values are just "no possible outcomes" while
   retained values represent "the only possible outcome".

     To represent that "no value" is a possible outcome, @a@
   should be some type like (@'Maybe' a@) or (@'Either' 'String' a@).

   [@f@]: This 'Functor' defines the broad scale
       behaviours but its 'Alternative' instance is overridden.
       This in particular might change during upcoming design
       validation.
   [@g@]: This 'Functor' supplies the supercedent 'Alternative'
       instance and thus the finer behaviours.
-}
points :: 
  (Functor f, Alternative g)
    => (a -> Bool)   -- ^ A predicate that indicates whether a point
                     --  is occupied by its original value or vacant.
    -> f a           -- ^ The seed points with their values.
    -> Compose f g a -- ^ The constructed @'SubZero'@ value.
points f d = keepIdentity f <-$> reveal d

{- $destructors
-}

{- | If the type constructor of the possibilities concept is @'Maybe'@
    then you can use @'flatten'@ to provide default values for
    impossible points.

    - /NOTE/: This uses the applicative instance of the broad scale
        @'Functor'@ which means exact behaviour can vary depending on
        the type of @'Applicative' f@ because each has a different technique
        to ensure a value is found for every point:

        [@list of a@]:    Cross-product; Providing all default values once
            for all points.
        [@'ZipList' a@]:  zipWith; Providing one default value for each
            point until there are either no defaults remaining
            or no more points.
        [@'Data.Functor.Identity.Identity' a@]: One default must surely
            be provided and it is used if a default is required.
        [@'Maybe' a@]:    Not sure exactly what this does, TBC.
        [@'Either' a@]:   Not sure exactly what this does, TBC.
-}
flatten ::
  (Applicative f)
    => f a               -- ^ Default values
    -> Compose f Maybe a -- ^ Structured container
    -> f a               -- ^ Destructured container
flatten a (Compose b) = fromMaybe <$> a <*> b

{-  $restructors
-}

{- | fmap below the zeropoint
-}
(<-$>) :: (Functor f) => (g a -> h a) -> Compose f g a -> Compose f h a
f <-$> (Compose o) = Compose $ f <$> o

{- | fmap below the zeropoint, function variant of '<-$>' operator
-}
universal f s = f <-$> s

{- | Alternative below the zeropoint
-}
(Compose a) <-|> (Compose b) = Compose $ (<|>) <$> a <*> b

-- {- | Take the alternatives embedded in the @'SubZero'@ and collapse them
--     with a combining function to a single @'Alternative'@ value or empty which
--     means no possible outcomes.
--      It's compatible with @'Maybe'@ for further uses with @'flatten'@, but you can
--     retain behaviours of more sophisticated types. A consequence of this
--     is that you will probably need to state a type.
-- -}

{- |
    prop> collapse f empty = simplify empty
    prop> collapse f (pure x) = simplify (pure x)
    prop> collapse f (x <|> y) = pure (f x y)

    @g@ must form a monoid under @f@ when @'empty'@ is the monoid's identity.
-}
class (Applicative g, Applicative h) => Superposition g h where
  {- | Tries to convert from one alternative to another
  -}
  simplify :: g a -> Maybe (h a)

  {- | Combines many alternatives into one using a function parameter.
      @'empty'@ stays @'empty'@. This is quite free in the type of the result so
      the user can choose whether to keep the same type to re-expand the
      structure or to transform to a smaller type to avoid relying on
      instances for certain behaviours.
  -}
  collapse :: (a -> a -> a) -- ^ combining function
           -> g a -- ^ full structure
           -> h a -- ^ collapsed structure

{- | Superposition within a nondeterminism list (ie, [])
    This is roughly the same as
    @'Superposition' ('Compose' 'Data.Functor.Identity.Identity' []) h@
    but of course they have different instances of other typeclasses and
    if that were really true then it would be Superpositions of Identity
    all the way down.
-}
instance (Alternative h) => Superposition [] h where
  simplify   []   = Just empty 
  simplify (a:[]) = Just $ pure a
  simplify   _    = Nothing
  collapse _ []   = empty
  collapse f l    = pure $ foldl1 f l

{- | Superposition within @'Compose' * g@
-}
instance (Applicative f, Superposition g h) => Superposition (Compose f g) (Compose f h) where
  -- | TODO: universal simplify, but if any points are @'Nothing'@ then the whole is @'Nothing'@
  simplify   = undefined 
  -- | universal collapse, each point is collapsed
  collapse f = universal $ collapse f

{-  $instructors

    Facilities, independent of SubZero, which are particularly useful when
    applied with SubZero and its specific associated functions. They
    provide details of how to achieve something in the other sections.

-}

{- | Turns a value "@a@" to @'Just' a@ or @'Nothing'@ based on a predicate assuming you use it in a
   context that wants @'Maybe' a@ instead of some other representation of @'Alternative'@s
-}
keep :: (Alternative f) => (a -> Bool) -> a -> f a
keep f x | f x       = pure x
         | otherwise = empty

{- | Does the same as 'keep' for values at the other end of 'Identity'.

   prop> keep = (keepIdentity . Identity) f
-}
keepIdentity :: (Alternative f) => (a -> Bool) -> Identity a -> f a
keepIdentity f x = keep f $ runIdentity x

