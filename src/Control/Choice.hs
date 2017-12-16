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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
    Module      : Control.Choice
    Description : Examples using SubZero
    Copyright   : (c) Tristan Wibberley, 2017
    License     : GPL-2
    Maintainer  : tristan.wibberley@gmail.com
    Stability   : experimental

    Here is a longer description of this module, containing some
    commentary with @some markup@.
-}
module Control.Choice
    ( TotalChoice( (<?>), (<?!>), Total )
    ) where

import Control.Applicative.SubZero
import Data.Functor.Compose
import Control.Applicative
import Data.Maybe

infixl 4 <?>, <?!>

{- | A class supporting choosing things. You can fill in missing items,
     add progressively less preferred choices (for some @c@ but not others),
     and make a final decision for all items.

     The operations are idempotent, that is, choices are not supplanted by
     later thoughts. This is important because plans for parallel computation
     can begin as soon as a choice has been made.
-}
class TotalChoice c where
    -- | This is @c@ with the part that supports choice removed
    type Total c a
    -- | choose some remaining items
    (<?>)  :: c a ->       c a ->       c a
    -- | choose all remaining items
    (<?!>) :: c a -> Total c a -> Total c a

-- | 'Maybe' works naturally because its 'Alternative' instance is a single
--   idempotent choice.
instance TotalChoice Maybe where
    type Total Maybe a = a
    (<?>)  = (<|>)
    (<?!>) = flip fromMaybe

-- | List works naturally because its 'Alternative' instance adds only
--   inferior choices. An empty list and the 'Prelude.head' are isomorphic to
--   'Prelude.Nothing' and 'Prelude.Just' respectively.
instance TotalChoice [] where
    type Total [] a = a
    (<?>)        = (<|>)
    []    <?!> b = b
    (a:_) <?!> _ = a

-- | 'Compose' is a little less natural and perhaps there should be a newtype.
--   The outer 'Applicative' acts like a collection of points and the inner
--   'Data.Functor.Functor' / 'TotalChoice' acts like the (pending) selection
--   for each point.
--
--   This starts as @'Compose' f d a@ and finishes as @f a@ but I'm not sure
--   if it would be better to finish as @'Compose' f
--   'Data.Functor.Identity.Identity' a@
instance (Applicative f, TotalChoice d) => TotalChoice (Compose f d) where
    type Total (Compose f d) a = f (Total d a)
    a <?> b                    = (<?>) <-$> a <-*> b
    (Compose a) <?!> b         = (<?!>) <$> a <*> b


