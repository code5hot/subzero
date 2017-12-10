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

{- | An applicative map that discards mismatched keys
     ie, it uses intersection for it's applicative instance

     It has a case in which every key is occupied and equal
-}
newtype IMap m v = IMap (Either v (m v))
  deriving (Show, Eq)

{- | A class for maps so IMap can be generic
-}
class IsMap m where
  type Key m
  fromList         :: [(Key m, v)] -> m v
  intersectionWith :: (a -> b -> c) -> m a -> m b -> m c
  mapmap           :: (a -> b) -> m a -> m b

instance Ord k => IsMap (M.Map k) where
  type Key (M.Map k) = k
  fromList           = M.fromList
  intersectionWith   = M.intersectionWith
  mapmap             = M.map

instance IsMap IM.IntMap where
  type Key IM.IntMap = IM.Key
  fromList           = IM.fromList
  intersectionWith   = IM.intersectionWith
  mapmap             = IM.map

instance IsMap m => IsMap (IMap m) where
  type Key (IMap m) = Key m
  fromList l        = IMap (Right $ fromList l)

  intersectionWith f (IMap (Left  u)) (IMap (Left  v)) = IMap (Left $ f u v)
  intersectionWith f (IMap (Left  u)) (IMap (Right b)) = IMap (Right $ mapmap (     f u) b)
  intersectionWith f (IMap (Right a)) (IMap (Left  v)) = IMap (Right $ mapmap (flip f v) a)
  intersectionWith f (IMap (Right a)) (IMap (Right b)) = IMap (Right $ intersectionWith f a b)

  mapmap f (IMap (Left  v)) = IMap (Left $ f v)
  mapmap f (IMap (Right a)) = IMap (Right $ mapmap f a)
 
instance IsMap m => Functor (IMap m) where
  fmap = mapmap

instance IsMap m => Applicative (IMap m) where
  pure v = IMap (Left v)
  (<*>)  = intersectionWith ($)

