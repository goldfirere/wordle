{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.Vector as V

import Words
import Wordle

main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests"
  [ unitTests
  , propertyBasedTests ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "filter peach" $
      filterWords peachState finalWords @?=
        ["marsh", "laugh", "rajah", "faith"]
   , testCase "respondToGuess" $
       respondToGuess "peach" "party" @?= "GXYXX" ]

propertyBasedTests :: TestTree
propertyBasedTests = testGroup "Property tests"
  [ testProperty "wordle property" wordleProperty ]

instance Arbitrary WordleWord where
  arbitrary = do
    index <- chooseInt (0, V.length finalWords - 1)
    return (finalWords V.! index)

wordleProperty guess answer
  = let response = respondToGuess guess answer
        new_state = advanceState startingState guess response
        filtered = filterWords new_state finalWords
    in
    answer `V.elem` filtered