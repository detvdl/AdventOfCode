module Lib
    ( solve
    ) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.List.Split     (splitOn)
import qualified Data.List as L
import Control.Applicative
import Control.Monad
import Data.Maybe

type Point = (Int, Int)
type Line = Set Point

parseMovements :: String -> [Point -> Point]
parseMovements d = replicate (read steps) stepFunc
  where (dir, steps) = splitAt 1 d
        stepFunc = case dir of
          "U" -> (\(y, z) -> (y, z + 1))
          "D" -> (\(y, z) -> (y, z - 1))
          "L" -> (\(y, z) -> (y - 1, z))
          "R" -> (\(y, z) -> (y + 1, z))
          otherwise -> id

move :: Point -> String -> [Point]
move origin mvmt = L.scanl' (flip ($)) origin $ parseMovements mvmt

genLine :: Point -> [String] -> [Point]
genLine _ [] = []
genLine origin (m:ms) = fstLine ++ genLine newOrigin ms
  where fstLine = move origin m
        newOrigin = last fstLine

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

solve :: IO()
solve = do
  contents <- readFile "input.txt"
  let [line1, line2] = map (genLine (0, 0)) . map (splitOn ",") $ lines contents
      ints = S.delete (0, 0) $ S.intersection (S.fromList line1) (S.fromList line2)
      dists = S.map (manhattan (0, 0)) ints
      totalSteps x = sum <$> sequence [L.elemIndex x line1, L.elemIndex x line2]
  print . fromJust . S.lookupMin $ dists
  print . minimum . S.toList $ S.map totalSteps ints
