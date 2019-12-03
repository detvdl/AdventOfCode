{-# LANGUAGE FlexibleInstances #-}

module Lib
    ( solve
    ) where

import qualified Data.List as L
import qualified Data.Set as S
import Data.Maybe

type FreqFunc = Integer -> Integer

instance Read FreqFunc where
  readsPrec _ input = case splitAt 1 input of
    ("+", num) -> [(\x -> x + read num::Integer, "")]
    ("-", num) -> [(\x -> x - read num::Integer, "")]
    otherwise -> []

cumulateFreq :: [FreqFunc] -> Integer -> Integer
cumulateFreq = foldl (.) id

inter :: [FreqFunc] -> [Integer]
inter = L.scanl' (flip ($)) 0

solve2 :: [Integer] -> Maybe Integer
solve2 = run S.empty
  where run _ [] = Nothing
        run v (x:xs) = if x `S.member` v then Just x else run (S.insert x v) xs

solve :: IO ()
solve = do
  contents <- readFile "input.txt"
  let freqs = map read $ lines contents
  print $ cumulateFreq freqs 0
  print . fromJust . solve2 . inter . cycle $ freqs
