{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.Vector as V

import Words
import Wordle
import Response

main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests"
  [ unitTests
  , propertyBasedTests ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "filter peach" $
      filterWords peachState finalWords @?=
        ["marsh", "laugh", "rajah", "harsh", "faith"]
   , testCase "respondToGuess" $
       respondToGuess "peach" "party" @?= "GXYXX" ]

propertyBasedTests :: TestTree
propertyBasedTests = localOption (QuickCheckReplay (Just 12345)) $
  testGroup "Property tests"
  [ testProperty "wordle word constructor" wordleWordConstruction
  , testProperty "wordle word deconstruction" wordleWordDestruction
  , testProperty "wordle property" wordleProperty
  , testProperty "same response" validWordsWithSameResponse ]

instance Arbitrary a => Arbitrary (Vec5 a) where
  arbitrary = mkVec5 <$> V.replicateM wordleWordLength arbitrary

instance Arbitrary WordleWord where
  arbitrary = do
    index <- chooseInt (0, V.length finalWords - 1)
    return (finalWords V.! index)

-- for a given guess and answer, the answer must remain a valid possibility after filtering
-- based on the response

wordleProperty guess answer
  = let response = respondToGuess guess answer
        new_state = advanceState startingState guess response
        filtered = filterWords new_state finalWords
    in
    answer `V.elem` filtered

validWordsWithSameResponse guess answer
  = let response = respondToGuess guess answer
        new_state = advanceState startingState guess response
        filtered = filterWords new_state finalWords
    in
    all (\ filtered_word -> respondToGuess guess filtered_word == response) filtered

wordleWordConstruction chars = getLetters (mkWordleWord chars) == chars
wordleWordDestruction word = mkWordleWord (getLetters word) == word

{-
guess: papal
answer: pupal
response = GXGGG
-}