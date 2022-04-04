{-# LANGUAGE BangPatterns #-}
module Response ( LetterResponse(..), Response, getResponses, respondToGuess, getResponseWordRep  ) where

import qualified Data.Foldable   as F
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Vector     as V

import           Data.String

import           Words

-------------------------
-- responses to guesses

data LetterResponse = Gray | Yellow | Green
  deriving (Eq, Ord)

instance Show LetterResponse where
  show Gray   = "X"
  show Yellow = "Y"
  show Green  = "G"

newtype Response = MkResponse (WordRep LetterResponse)
  deriving (Eq, Ord)

getResponses :: Response -> Vec5 LetterResponse
getResponses (MkResponse rs) = getVec5FromWordRep rs

getResponseWordRep :: Response -> WordRep LetterResponse
getResponseWordRep (MkResponse rs) = rs

instance Show Response where
  show response = concatMap show (V.toList $ getVector $ getResponses response)

instance IsString Response where
  fromString input
    | length input == wordleWordLength = MkResponse ((mkWordRepFromVec5 $ mkVec5 $ V.fromList (map convert_letter_response input)) <> M.fromList [(Gray, mempty), (Green, mempty), (Yellow, mempty)])
    | otherwise = error "bad length"
    where
      convert_letter_response 'G' = Green
      convert_letter_response 'Y' = Yellow
      convert_letter_response 'X' = Gray
      convert_letter_response _   = error "invalid letter"

respondToGuess :: WordleWord   -- guess
               -> WordleWord   -- answer
               -> Response     -- pattern of gray, yellow, and green boxes
respondToGuess (MkWord guess_rep) (MkWord answer_rep) = MkResponse $
                                                        M.fromList [ (Green, green_locations)
                                                                   , (Yellow, yellow_locations)
                                                                   , (Gray, bad_locations <> some_gray_locations) ]
  where
    bad_locations :: S.Set Location
    bad_locations = F.fold (guess_rep `M.difference` answer_rep)

    (green_locations, yellow_locations, some_gray_locations)
     = F.fold (M.intersectionWith go guess_rep answer_rep)
      where
        go :: S.Set Location   -- locations of a given letter in the guess
           -> S.Set Location   -- locations of a given letter in the answer
           -> (S.Set Location, S.Set Location, S.Set Location)   -- green locations, yellow locations, some gray locations
        go !guess_locs !answer_locs =
          (green_locs, yellow_locs, gray_locs)
          where
              -- running example: "adapt" "adore"
              -- guess_locs: a: {0, 2}
              -- answer_locs: a: {0}
            !green_locs = answer_locs `S.intersection` guess_locs          -- a: {0}
            !answer_locs_not_green = answer_locs `S.difference` guess_locs -- a: {}
            !n_answer_locs_not_green = S.size answer_locs_not_green        -- a: 0
            !guess_locs_not_green = guess_locs `S.difference` green_locs   -- a: {2}

            !(!yellow_locs_list, !gray_locs_list) = splitAt n_answer_locs_not_green (S.toAscList guess_locs_not_green)
            !yellow_locs = S.fromList yellow_locs_list
            !gray_locs = S.fromList gray_locs_list
