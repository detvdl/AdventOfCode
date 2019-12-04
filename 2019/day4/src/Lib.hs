module Lib
    ( solve
    ) where

import           Data.List          (group, sort)
import           Data.List.Split    (splitOn)
import           System.Environment

isValidPwd :: (Int -> Bool) -> String -> Bool
isValidPwd pred x = (length x) == 6
                    && (sort x) == x
                    && any pred (map length (group x))

solve :: IO ()
solve = do
  (r:_) <- getArgs
  let [from, to] = map (read::String->Int) $ splitOn "-" r
  print $ length $ filter (isValidPwd (> 1)) . map show $ [from..to]
  print $ length $ filter (isValidPwd (== 2)) . map show $ [from..to]
