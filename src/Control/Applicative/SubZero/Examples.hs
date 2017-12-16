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

{-# LANGUAGE PartialTypeSignatures #-}

{- |
    Module      : Control.Applicative.SubZero.Examples
    Description : Examples using SubZero
    Copyright   : (c) Tristan Wibberley, 2017
    License     : GPL-2
    Maintainer  : tristan.wibberley@gmail.com
    Stability   : experimental

    Here is a longer description of this module, containing some
    commentary with @some markup@.
-}
module Control.Applicative.SubZero.Examples
    ( fizzbuzz
    , fizzbuzz'
    , fizzbuzz''
    , fizzbuzz'''
    , fizzbuzz''''
    ) where

import Control.Choice
import Control.Applicative.SubZero
import Data.Functor.Compose
import Control.Applicative
import Data.Functor.Identity

{- | Takes values in an 'Applicative' and returns the 
     <https://en.wikipedia.org/wiki/Fizz_buzz fizzbuzz game>'s answer
     for that value. /Caution/ regular lists will not do what you want, use:

     - 'Data.Functor.Identity.Identity'
     - 'Maybe'
     - 'Either'
     - 'Control.Applicative.ZipList'
     - 'Data.Map.IMap.IMap'
-}
fizzbuzz :: (Applicative f, Show n, Integral n) => f n -> f String
fizzbuzz indexes = let isMultiple n x = x `mod` n == 0

                       numbers    = show <$> indexes
                       fizzpoints = points (isMultiple 3) indexes
                       fizzes     = "Fizz" <$ fizzpoints
                       buzzpoints = points (isMultiple 5) indexes
                       buzzes     = "Buzz" <$ buzzpoints

                       substitutions :: Compose _ [] _
                       substitutions = fizzes <-|> buzzes

                       words :: Compose _ Maybe _
                       words = collapse (++) substitutions

                   in  words <?!> numbers

{- | Takes a pure value and returns the 
     <https://en.wikipedia.org/wiki/Fizz_buzz fizzbuzz game>'s answer
     for that value.
-}
fizzbuzz' :: (Show n, Integral n) => n -> String
fizzbuzz' index = let isMultiple n x = x `mod` n == 0

                      number = show index
                      fizzness = keep (isMultiple 3) index
                      fizz = "Fizz" <$ fizzness
                      buzzness = keep (isMultiple 5) index
                      buzz = "Buzz" <$ buzzness

                      substitutions :: [] _
                      substitutions = fizz <|> buzz

                      word :: Maybe _
                      word = collapse (++) substitutions

                  in  word <?!> number

fizzbuzz'' :: (Show n, Integral n) => n -> String
fizzbuzz'' = runIdentity . fizzbuzz . pure

fizzbuzz''' :: (Functor f, Show n, Integral n) => f n -> f String
fizzbuzz''' = fmap fizzbuzz'

fizzbuzz'''' :: (Functor f, Show n, Integral n) => f n -> f String
fizzbuzz'''' = fmap fizzbuzz''

