{-
SubZero, A haskell library to provide a useful data structure
Copyright (C) 2017 Tristan Wibberley, Eric Mertens

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

{- |
    Module      : Data.Map.IMap
    Description : Types, instances, and functions for an applicative Map
    Copyright   : (c) Tristan Wibberley, Eric Mertens, 2017
    License     : GPL-2
    Maintainer  : tristan.wibberley@gmail.com
    Stability   : experimental
-}
module Data.Map.IMap 
    ( IMap
    , fromList
    , intersectionWith
    ) where

import qualified Data.Map as M
import qualified Data.IntMap as IM

{- | An applicative map that discards mismatched keys (ie, uses intersection)
 - -}
newtype IMap m v = IMap (Either v (m v))
  deriving (Show, Eq)

class Functor m => IsMap m where
  type Key m
  fromList         :: [(Key m, v)] -> m v
  intersectionWith :: (a -> b -> c) -> m a -> m b -> m c


instance Ord k => IsMap (M.Map k) where
  type Key (M.Map k) = k
  fromList           = M.fromList
  intersectionWith   = M.intersectionWith

instance IsMap IM.IntMap where
  type Key IM.IntMap = IM.Key
  fromList           = IM.fromList
  intersectionWith   = IM.intersectionWith

instance Functor m => Functor (IMap m) where
  fmap f (IMap (Left  v)) = IMap (Left $ f v)
  fmap f (IMap (Right m)) = IMap (Right $ fmap f m)

instance IsMap m => Applicative (IMap m) where
  pure v                            = IMap (Left v)
  IMap (Left  f) <*> b              = fmap f b
  a              <*> IMap (Left  v) = fmap ($v) a
  IMap (Right m) <*> IMap (Right v) = IMap (Right $ intersectionWith ($) m v)

instance IsMap m => IsMap (IMap m) where
  type Key (IMap m) = Key m
  fromList l        = IMap (Right $ fromList l)
  intersectionWith  = undefined

