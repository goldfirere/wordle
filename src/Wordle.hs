module Wordle where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T

import Data.String

-- KNOWN BUG: Does not work for words with repeated letters

import Words

-------------------------
-- utility

lettersInWord :: WordleWord -> S.Set Char
lettersInWord word = V.foldl' (flip S.insert) S.empty (getVector $ getLetters word)

allLocations :: [Location]
allLocations = [0 .. 4]

-------------------------
-- responses to guesses

data LetterResponse = Gray | Yellow | Green
  deriving (Eq)

instance Show LetterResponse where
  show Gray = "X"
  show Yellow = "Y"
  show Green = "G"

newtype Response = MkResponse (Vec5 LetterResponse)
  deriving (Eq)

getResponses :: Response -> Vec5 LetterResponse
getResponses (MkResponse rs) = rs

instance Show Response where
  show (MkResponse responses) = concatMap show (V.toList $ getVector responses)

instance IsString Response where
  fromString input
    | length input == wordleWordLength = MkResponse (mkVec5 $ V.fromList (map convert_letter_response input))
    | otherwise = error "bad length"
    where
      convert_letter_response 'G' = Green
      convert_letter_response 'Y' = Yellow
      convert_letter_response 'X' = Gray
      convert_letter_response _   = error "invalid letter"

respondToGuess :: WordleWord   -- guess
               -> WordleWord   -- answer
               -> Response     -- pattern of gray, yellow, and green boxes
respondToGuess guess answer = MkResponse (vec5ZipWith convert green_spots (getLetters guess))
  where
    answer_letters = lettersInWord answer

    green_spots :: Vec5 Bool
    green_spots = vec5ZipWith (==) (getLetters guess) (getLetters answer)

    convert :: Bool -> Char -> LetterResponse
    convert False the_letter | the_letter `S.member` answer_letters = Yellow
                             | otherwise                            = Gray
    convert True  _ = Green


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
      = all letter_is_in_location yes_locations && all (not . letter_is_in_location) no_locations && letter `V.elem` getVector word_letters
      where
        letter_is_in_location :: Location -> Bool
        letter_is_in_location location = vec5Lookup word_letters location == letter
  {-
    required_letters = requiredLetters state

    -- check whether any letter is in a forbidden location
    no_letters_in_bad_locations :: WordleWord -> Bool
    no_letters_in_bad_locations word = V.ifoldl' go True (getLetters word)
      where
        go :: Bool -> Int -> Char -> Bool
        go False _index _letter = False
        go True index letter = case M.lookup letter mapping of
           -- letter hasn't been seen before:
          Nothing -> True
           -- see a letter that we know is not in the word. Reject!
          Just LetterNotInWord -> False
          Just (LetterNotInLocations excluded_locations) -> not (index `S.member` excluded_locations)

    -- all the required letters are indeed in the word
    all_required_letters :: WordleWord -> Bool
    all_required_letters word = required_letters `S.isSubsetOf` lettersInWord word
  -}

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