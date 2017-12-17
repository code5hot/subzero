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

import System.Exit
import Control.Applicative.SubZero.Examples
import Control.Applicative (ZipList(ZipList), getZipList)
import Data.Functor.Identity
import Data.Map.IMap
import Data.Map.Lazy (Map)
import Test.Hspec
import Control.Monad

main :: IO ()
main = hspec $ do
  describe "fizzbuzz" $ do
    describe "returns the fizzbuzz call for each given number" $ do
      it "does that for 'Identity'" $ do
        fizzbuzz (Identity  1) `shouldBe` Identity "1"
        fizzbuzz (Identity  2) `shouldBe` Identity "2"
        fizzbuzz (Identity  3) `shouldBe` Identity "Fizz"
        fizzbuzz (Identity  5) `shouldBe` Identity "Buzz"
        fizzbuzz (Identity 15) `shouldBe` Identity "FizzBuzz"

      it "does that for 'Maybe'" $ do
        fizzbuzz (Just  1) `shouldBe` Just "1"
        fizzbuzz (Just  2) `shouldBe` Just "2"
        fizzbuzz (Just  3) `shouldBe` Just "Fizz"
        fizzbuzz (Just  5) `shouldBe` Just "Buzz"
        fizzbuzz (Just 15) `shouldBe` Just "FizzBuzz"

      it "does that for 'Either'" $ do
        fizzbuzz (Right  1) `shouldBe` (Right "1"        :: Either () _)
        fizzbuzz (Right  2) `shouldBe` (Right "2"        :: Either () _)
        fizzbuzz (Right  3) `shouldBe` (Right "Fizz"     :: Either () _)
        fizzbuzz (Right  5) `shouldBe` (Right "Buzz"     :: Either () _)
        fizzbuzz (Right 15) `shouldBe` (Right "FizzBuzz" :: Either () _)
       
      it "does that for 'ZipList'" $ do
        let indexes  = ZipList $ take 15 [1 :: Integer ..]
            the_expected = ZipList [    "1",    "2",    "Fizz",    "4"
                                   , "Buzz", "Fizz",       "7",    "8"
                                   , "Fizz", "Buzz",      "11", "Fizz"
                                   ,   "13",   "14","FizzBuzz"]

        fizzbuzz indexes `shouldBe` the_expected

      it "does that for '[]'" $ do
        let indexes  = take 15 [1 :: Integer ..]
            the_expected = [    "1",    "2",    "Fizz",    "4"
                           , "Buzz", "Fizz",       "7",    "8"
                           , "Fizz", "Buzz",      "11", "Fizz"
                           ,   "13",   "14","FizzBuzz"]

        fizzbuzz indexes `shouldBe` the_expected

      it "does that for 'IMap'" $ do
        let indexes :: IMap (Map _) _
            indexes  = fromList $ join (,) <$> ([1,2,3,4,5,6,7,8,12,13,14,15] :: [Integer])
            the_expected = fromList [ ( 1,   "1"), ( 2,   "2"), ( 3,"Fizz"), ( 4,       "4")
                                    , ( 5,"Buzz"), ( 6,"Fizz"), ( 7,   "7"), ( 8,       "8")
                                    , (12,"Fizz"), (13,  "13"), (14,  "14"), (15,"FizzBuzz")]

        fizzbuzz indexes `shouldBe` the_expected

    it "passes through the failure value of the functor untouched" $ do
      fizzbuzz Nothing `shouldBe` Nothing
      fizzbuzz (Left "message") `shouldBe` Left "message"

