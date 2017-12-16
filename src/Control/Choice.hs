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

class TotalChoice c where
    type Total c a
    (<?>)  :: c a ->       c a ->       c a -- ^ choose some remaining
    (<?!>) :: c a -> Total c a -> Total c a -- ^ choose all remaining

instance TotalChoice Maybe where
    type Total Maybe a = a
    (<?>)  = (<|>)
    (<?!>) = flip fromMaybe

instance TotalChoice [] where
    type Total [] a = a
    (<?>)        = (<|>)
    []    <?!> b = b
    (a:_) <?!> _ = a

instance (Applicative f, TotalChoice d) => TotalChoice (Compose f d) where
    type Total (Compose f d) a = f (Total d a)
    a <?> b                    = (<?>) <-$> a <-*> b
    (Compose a) <?!> b         = (<?!>) <$> a <*> b


