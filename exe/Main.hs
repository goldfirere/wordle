module Main where

import Control.DeepSeq

import Words
import Wordle

main :: IO ()
main = print (maxExpectedValue finalWords)