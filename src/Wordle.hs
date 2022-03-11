module Wordle where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Foldable as F

import Data.String

import Words
import Response

-------------------------
-- Game state

-- for each letter in the alphabet, either:
--   1) we know nothing: not in the State map
--   2) it's not in the word
--   3) it's in the word, but not in some spots

data LetterInformation
  = LetterNotInWord
  | LetterInWord { here :: S.Set Location         -- set of places letter can *must* appear
                 , not_here :: S.Set Location }   -- set of places letter can *not* appear
  deriving Show

data State = MkState (M.Map Char LetterInformation)

instance Show State where
  show (MkState mapping) = show mapping

startingState :: State
startingState = MkState mempty

instance Semigroup State where
  (MkState map1) <> (MkState map2) = MkState (M.unionWith combine map1 map2)
    where
      combine :: LetterInformation -> LetterInformation -> LetterInformation
      combine LetterNotInWord LetterNotInWord = LetterNotInWord
      combine LetterNotInWord (LetterInWord {}) = error "contradictory info (1)"
      combine (LetterInWord {}) LetterNotInWord = error "contradictory info (2)"
      combine (LetterInWord { here = here1, not_here = not_here1 })
              (LetterInWord { here = here2, not_here = not_here2 })
              = LetterInWord { here = here1 <> here2, not_here = not_here1 <> not_here2 }

instance Monoid State where
  mempty = startingState

-------------------------------------
-- advancing state

advanceState :: State -> WordleWord -> Response -> State
advanceState state word response = state <> new_state
  where
     -- a new State reflecting only information in the Response
    new_state = MkState new_mapping

    guess_with_response :: Vec5 (Char, LetterResponse)
    guess_with_response = vec5Zip (getLetters word) (getResponses response)

    new_mapping = V.ifoldl' go mempty (getVector guess_with_response)
      where
        go :: M.Map Char LetterInformation -> Int -> (Char, LetterResponse) -> M.Map Char LetterInformation
        go old_mapping _     (letter, Gray)   = M.insert letter LetterNotInWord old_mapping
        go old_mapping index (letter, Yellow) = M.insert letter (LetterInWord { here = mempty, not_here = S.singleton index }) old_mapping
        go old_mapping index (letter, Green)  = M.insert letter (LetterInWord { here = S.singleton index, not_here = mempty }) old_mapping

--------------------------------------
-- filtering

-- set of all letters that are required, according to the state
{-
requiredLetters :: State -> S.Set Char
requiredLetters (MkState mapping) = M.foldlWithKey' go S.empty mapping
  where
    go :: S.Set Char -> Char -> LetterInformation -> S.Set Char
    go already_required _current_letter LetterNotInWord = already_required
    go already_required current_letter (LetterNotInLocations {}) = current_letter `S.insert` already_required
-}

validWord :: State -> WordleWord -> Bool
validWord (MkState mapping) word = all check_one_mapping mappings
  -- no_letters_in_bad_locations word && all_required_letters word
  where
    word_letters :: Vec5 Char
    word_letters = getLetters word

    mappings :: [(Char, LetterInformation)]
    mappings = M.toList mapping

    check_one_mapping :: (Char, LetterInformation) -> Bool
    check_one_mapping (letter, LetterNotInWord) = not (letter `V.elem` getVector word_letters)
    check_one_mapping (letter, LetterInWord { here = yes_locations, not_here = no_locations })
      = all letter_is_in_location yes_locations && not (any letter_is_in_location no_locations) && letter `V.elem` getVector word_letters
      where
        letter_is_in_location :: Location -> Bool
        letter_is_in_location location = vec5Lookup word_letters location == letter

filterWords :: State -> V.Vector WordleWord -> V.Vector WordleWord
filterWords state words = V.filter (validWord state) words

partitionWords :: State -> V.Vector WordleWord -> (V.Vector WordleWord, V.Vector WordleWord)
partitionWords state words = V.partition (validWord state) words

------------------
-- examples

-- Suppose we get XXYXG for a guess of PEACH
peachState :: State
peachState = MkState (M.fromList [ ('p', LetterNotInWord)
                                 , ('e', LetterNotInWord)
                                 , ('a', LetterInWord { here = mempty, not_here = S.singleton 2 })
                                 , ('c', LetterNotInWord)
                                 , ('h', LetterInWord { here = S.singleton 4, not_here = mempty })])