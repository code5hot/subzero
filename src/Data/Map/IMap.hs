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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
    Module      : Data.Map.IMap
    Description : Types, instances, and functions for an applicative Map
    Copyright   : (c) Tristan Wibberley, 2017
    License     : GPL-2
    Maintainer  : tristan.wibberley@gmail.com
    Stability   : experimental
-}
module Data.Map.IMap 
    ( IMap
    , fromList
    ) where

import qualified Data.Map as M

{- | An applicative map that discards mismatched keys (ie, uses intersection)
-}
newtype IMap m k v = IMap (Either v (m k v))
  deriving (Show, Eq)

instance Functor (m k) => Functor (IMap m k) where
  fmap f (IMap (Left v))  = IMap (Left $ f v)
  fmap f (IMap (Right m)) = IMap (Right $ fmap f m)

instance (Functor (M.Map k), Ord k) => Applicative (IMap M.Map k) where
  pure v                                  = IMap (Left v)
  (IMap (Left f))    <*> b                = fmap f b
  a@(IMap (Right m)) <*> (IMap (Left v))  = fmap ($v) a
  (IMap (Right m))   <*> (IMap (Right v)) = IMap (Right $ M.intersectionWith ($) m v)

fromList l = IMap (Right $ M.fromList l)
