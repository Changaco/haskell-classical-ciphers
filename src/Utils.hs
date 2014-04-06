module Utils where

import Prelude

import qualified Data.Set as Set


nubOrd :: (Ord a) => [a] -> [a]
nubOrd = go Set.empty
  where go _ [] = []
        go s (x:xs) | Set.member x s = go s xs
                    | otherwise      = x : go (Set.insert x s) xs
