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
    Module      : Data.Map.Class
    Description : Class and instances for map types from the containers package
    Copyright   : (c) Tristan Wibberley, Eric Mertens, 2017
                : (c) Daan Leijen 2002
                : (c) Andriy Palamarchuk 2008
    License     : GPL-2
    Maintainer  : tristan.wibberley@gmail.com
    Stability   : experimental
-}
module Data.Map.Class
    ( IsMap
    , fromList
    , intersectionWith
    , mapmap
    , Key
      {--------------------------------------------------------------------
       -   Operators
       --------------------------------------------------------------------}
    , (!), (!?), (\\)
    ) where

import qualified Data.Map as M
import qualified Data.IntMap as IM

infixl 9 !,!?,\\

{- | A class for maps so map types can be generic
-}
class IsMap m where
  type Key m
  instance
  fromList         :: [(Key m, v)] -> m v
  intersectionWith :: (a -> b -> c) -> m a -> m b -> m c
  mapmap           :: (a -> b) -> m a -> m b

  -- | /O(log n)/. Lookup the value at a key in the map.
  --
  -- The function will return the corresponding value as @('Just' value)@,
  -- or 'Nothing' if the key isn't in the map.
  --
  -- An example of using @lookup@:
  --
  -- > import Prelude hiding (lookup)
  -- > import Data.Map.Class
  -- >
  -- > employeeDept = fromList([("John","Sales"), ("Bob","IT")])
  -- > deptCountry = fromList([("IT","USA"), ("Sales","France")])
  -- > countryCurrency = fromList([("USA", "Dollar"), ("France", "Euro")])
  -- >
  -- > employeeCurrency :: String -> Maybe String
  -- > employeeCurrency name = do
  -- >     dept <- lookup name employeeDept
  -- >     country <- lookup dept deptCountry
  -- >     lookup country countryCurrency
  -- >
  -- > main = do
  -- >     putStrLn $ "John's currency: " ++ (show (employeeCurrency "John"))
  -- >     putStrLn $ "Pete's currency: " ++ (show (employeeCurrency "Pete"))
  --
  -- The output of this program:
  --
  -- >   John's currency: Just "Euro"
  -- >   Pete's currency: Nothing
  lookup :: Ord k => Key m -> m a -> Maybe a
 
  -- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
  --
  -- > member 5 (fromList [(5,'a'), (3,'b')]) == True
  -- > member 1 (fromList [(5,'a'), (3,'b')]) == False
  member :: Ord k => k -> Map k a -> Bool

(...) = ((.).(.))
(!?) = flip lookup
(!)  = maybe (error "can't find key") ... (!?)

instance Ord k => IsMap (M.Map k) where
  type Key (M.Map k) = k
  fromList           = M.fromList
  intersectionWith   = M.intersectionWith
  mapmap             = M.map
  (\\)               = (M.\\)
  lookup             = M.lookup
  member             = M.member

instance IsMap IM.IntMap where
  type Key IM.IntMap = IM.Key
  fromList           = IM.fromList
  intersectionWith   = IM.intersectionWith
  mapmap             = IM.map
  (\\)               = (IM.!?)
  lookup             = IM.lookup
  member             = IM.member

