{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Wordle where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.List as L

import Data.Function ( on )

import Data.String

import Words
import Response
import Data.Maybe

-------------------------
-- Game state

data Guess = MkGuess WordleWord Response

instance Show Guess where
  show (MkGuess guess response) = show guess ++ " --> " ++ show response

newtype State = MkState [Guess]
  deriving newtype (Show, Monoid, Semigroup)

startingState :: State
startingState = mempty

-------------------------------------
-- advancing state

advanceState :: State -> WordleWord -> Response -> State
advanceState state word response = MkState [MkGuess word response] <> state

--------------------------------------
-- filtering

validWord :: State -> WordleWord -> Bool
validWord (MkState guesses) word = map (`respondToGuess` word) guess_words == guess_responses
  where
    (guess_words, guess_responses) = unzipGuesses guesses

unzipGuesses :: [Guess] -> ([WordleWord], [Response])
unzipGuesses [] = ([], [])
unzipGuesses (MkGuess word response : guesses) = (word : words, response : responses)
  where
    (words, responses) = unzipGuesses guesses

filterWords :: State -> V.Vector WordleWord -> V.Vector WordleWord
filterWords state words = V.filter (validWord state) words

-------------------------------
-- finding a good guess

class Answer answer where
  include :: WordleWord -> answer -> answer
  maybeToAnswer :: Maybe answer -> answer
instance Answer [WordleWord] where
  include = (:)
  maybeToAnswer Nothing = []
  maybeToAnswer (Just words) = words
instance Answer Int where
  include _ = (+1)
  maybeToAnswer Nothing = 0
  maybeToAnswer (Just n) = n

partitionWords :: forall answer
                . Answer answer
               => WordleWord            -- possible guess
               -> V.Vector WordleWord   -- possible answers
               -> [answer] -- partitioned answers; each word in a given sub-list
                           -- provides the *same* answer for the given guess
partitionWords guess possible_answers = M.elems (F.foldl' go mempty possible_answers)
  where
    go :: M.Map Response answer -> WordleWord -> M.Map Response answer
    go acc possible_answer = M.alter alter_fun (respondToGuess guess possible_answer) acc
      where
        alter_fun :: Maybe answer -> Maybe answer
        alter_fun old_entry = Just (include possible_answer (maybeToAnswer old_entry))
{-
[ [a,b,c], [d,e], [f], [g,h,i,j,k,l] ]
[ 3, 2, 1, 6 ]    total: 12
expected value:
sum [ 3/12 * 9, 2/12 * 10, 1/12 * 11, 6/12 * 6 ]
-}

getLengths :: [[a]] -> [Int]
getLengths = map length

expectedNumberOfEliminatedWords :: Int    -- total # of elements
                                -> [Int]  -- lengths of partitions
                                -> Double
expectedNumberOfEliminatedWords total lengths = sum [ len / total_double * (total_double - len) | len <- map fromIntegral lengths ]
  where
    total_double = fromIntegral total

expectedValueOfGuess :: WordleWord -> V.Vector WordleWord -> Double
expectedValueOfGuess word possible_words = expectedNumberOfEliminatedWords (V.length possible_words)
                                             (partitionWords word possible_words)

maxExpectedValue :: V.Vector WordleWord -> WordleWord
maxExpectedValue possible_words = fst $ F.maximumBy (compare `on` snd)
                                  [ (guess, value)
                                  | guess <- V.toList possible_words
                                  , let value = expectedValueOfGuess guess possible_words ]

{-
for a possible guess, we want to partition the set of available words based on what response they give

suppose: guess = "peach"
possible words: [ "party", "beach", "house", "patxs" ]
if the answer is "party", then guess "peach" gives me GXYXX

-}

