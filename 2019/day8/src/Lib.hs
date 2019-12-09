module Lib
    ( solve
    ) where

import           Control.Applicative
import qualified Data.List           as L
import           Data.List.Split     (chunksOf)

buildImg :: Int -> Int -> String -> [String]
buildImg x y = chunksOf (x * y)

count :: (Eq a) => a -> [a] -> Int
count = (length .) . filter . (==)

leastZero :: [String] -> String
leastZero = L.minimumBy (\a b -> compare (zeros a) (zeros b))
  where zeros = count '0'

pAnd :: Char -> Char -> Char
pAnd '2' x = x
pAnd y x   = y

combineLayers :: String -> String -> String
combineLayers = zipWith pAnd

flattenImg :: [String] -> String
flattenImg = foldl1 combineLayers

solve :: IO ()
solve = do
  contents <- readFile "input.txt"
  let (line:_) = lines contents
      img = buildImg 25 6 line
  print $ product $ [(count '1'), (count '2')] <*> pure (leastZero img)
  putStr $ unlines . chunksOf 25 . map (\x -> if x == '0' then ' ' else '#') $ flattenImg img
