module Main where

fuel :: Integer -> Integer
fuel = subtract 2 . flip div 3

exhaust :: Integer -> Integer
exhaust = sum . drop 1 . takeWhile (> 0) . iterate fuel

main :: IO()
main = do
  content <- readFile "input.txt"
  let contentLines = lines content
  let intLines = map (\x -> read x::Integer) contentLines
  print . sum . map fuel $ intLines
  print . sum . map exhaust $ intLines
