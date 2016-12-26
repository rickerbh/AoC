module Frequency
  ( frequencySort,
    letterFrequency,
  ) where

import Data.List (sortBy)
import Data.Map (toList, fromListWith)

frequencySort :: (Ord k, Ord a) => [(k, a)] -> [(k, a)]
frequencySort xs = reverse $ sortBy frequencySort' xs

frequencySort' :: (Ord k, Ord a) => (k, a) -> (k, a) -> Ordering
frequencySort' a b
  | freqComp == EQ = compare (fst b) (fst a)
  | otherwise      = freqComp
  where
    freqComp = compare (snd a) (snd b)

letterFrequency :: (Ord k, Num a) => [k] -> [(k, a)]
letterFrequency xs = toList $ fromListWith (+) [(c, 1) | c <- xs]
