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

import Control.Applicative
import Control.Applicative.SubZero
import System.Exit

main :: IO ()

fizzbuzz indexes = let isMultiple n x = x `mod` n == 0 in

                   let numbers = show <$> indexes in
                   let fizzpoints = points (isMultiple 3) indexes in
                   let fizzes = "Fizz" <$ fizzpoints in
                   let buzzpoints = points (isMultiple 5) indexes in
                   let buzzes = "Buzz" <$ buzzpoints in


                   let substitutions :: SubZero _ [] _
                       substitutions = fizzes <|> buzzes in

                   let words = collapse (++) substitutions in
                   fromSubZero numbers words

main = let indexes = ZipList [1..] in -- mostly lattice-like uses, not nondeterminism
       let result = fizzbuzz indexes in
       let check_answers = take 15 $ getZipList $ result in
       do putStrLn $ show check_answers
          if check_answers == ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz"]
           then exitSuccess
           else exitFailure
