{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Response

-- Our benchmark harness.
main = defaultMain [
  bgroup "respond" [ bench "peach/party"  $ whnf (respondToGuess "peach") "party"
                   , bench "aioli/fries"  $ whnf (respondToGuess "aioli") "fries"
                   , bench "bench/patty"  $ whnf (respondToGuess "bench") "patty"
                   ]
  ]

-- whnf = weak head normal form