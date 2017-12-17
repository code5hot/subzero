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
    Module      : Control.Zippy
    Description : Class for zippy functors and zippifying some other functors
    Copyright   : (c) Tristan Wibberley, 2017
    License     : GPL-2
    Maintainer  : tristan.wibberley@gmail.com
    Stability   : experimental

    This module provides a class for zippy functors and a class for
    zippifying other functors that could have been zippy but aren't.
-}
module Control.Zippy
    ( Zippy( (<:>) )
    ) where

import Data.Functor.Compose
import Data.Maybe
import Data.Functor.Identity
import Control.Applicative
import Data.Map.IMap

infixl 4 <:>

{- |
     prop> pure f <:> pure x = fmap f (pure x)

     if @z@ is also an 'Alternative':

     prop> (pure f <|> pure g) <:> (pure x <|> pure y) = (pure f <:> pure x) <|> (pure g <:> pure y)
-}
class Functor z => Zippy z where
    -- | Zippy pure
    zpure :: a -> z a
    -- | Zippy application == <*>
    (<:>)  :: z (a -> b) -> z a -> z b

instance Zippy Identity where
    zpure = pure
    (<:>) = (<*>)
instance Zippy Maybe where
    zpure = pure
    (<:>) = (<*>)
instance Zippy (Either a) where
    zpure = pure
    (<:>) = (<*>)
instance Zippy ZipList where
    zpure = pure
    (<:>) = (<*>)
instance IsMap m => Zippy (IMap m) where
    zpure = pure
    (<:>) = (<*>)
instance Zippy [] where
    zpure = repeat
    (<:>) = zipWith ($)

