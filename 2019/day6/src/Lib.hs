module Lib
    ( solve
    ) where

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM
import qualified Data.Tree as T
import Data.Tree (Tree(..))
import Data.Maybe
import Control.Monad
import Control.Applicative

type Orbit = (String, String)

buildTree :: String -> [String] -> Tree String
buildTree root orbits = T.unfoldTree buildNode root
  where buildNode n = (n, children n)
        children node = MM.lookup node orbitMap
        orbitMap = buildMap orbits

buildMap :: [String] -> MultiMap String String
buildMap = foldr (insertOrbit . parseOrbit) MM.empty

parseOrbit :: String -> Orbit
parseOrbit = fmap (drop 1) . break (== ')')

insertOrbit :: Orbit -> MultiMap String String -> MultiMap String String
insertOrbit = uncurry MM.insert

depth :: String -> Tree String -> Maybe Int
depth x tree = go x 0 tree
  where go el level (Node p ts)
          | p == el = Just level
          | ts == [] = Nothing
          | otherwise = msum $ fmap (go el (level + 1)) ts

-- Shortest path from n1 to n2 is
-- Dist(root, n1) + Dist(root, n2) - 2 * Dist(root, LCA(n1, n2))
-- But we have to exclude the nodes themselves from the path
-- Hence minus another 2
shortestPath :: String -> String -> Tree String -> Maybe Int
shortestPath x y t = (-) <$> pathLength <*> Just 2
  where pathLength = (-) <$> leafDepth <*> lcaDepth
        lcaDepth = (*2) <$> snd <$> findLca x y t
        leafDepth = liftA2 (+) (depth x t) (depth y t)

findLca :: String -> String -> Tree String -> Maybe(String, Int)
findLca = findLca' 0

findLca' :: Int -> String -> String -> Tree String -> Maybe(String, Int)
findLca' d x y (Node p _)
  | p == x || p == y = Just (p, d)
findLca' _ x y (Node p []) = Nothing
findLca' d x y (Node p (l:[])) = findLca' (d + 1) x y l
findLca' d x y (Node p (l:r:_)) =
  case (findLca' (d + 1) x y l, findLca' (d + 1) x y r) of
    (Nothing, Nothing) -> Nothing
    (Just _, Just _) -> Just (p, d)
    (Nothing, Just u) -> Just u
    (Just u, Nothing) -> Just u

solve :: IO ()
solve = do
  contents <- readFile "input.txt"
  let orbits = lines contents
      tree = buildTree "COM" $ orbits
      levels = T.levels tree
      sumTree (level, depth) = (+) ((length level) * depth)
  print $ foldr sumTree 0 $ zip (drop 1 levels) [1..]
  print $ fromJust $ shortestPath "YOU" "SAN" tree
