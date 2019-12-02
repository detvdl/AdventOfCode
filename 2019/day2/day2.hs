module Main where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Applicative (liftA2)
import Data.Maybe
import Data.List.Split

{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

intCode :: Int -> Seq Int -> Seq  Int
intCode pos xs = case Seq.lookup pos xs of
  Just 1 -> intCode (pos + 4) . Seq.update newPos sumVal $ xs
  Just 2 -> intCode (pos + 4) . Seq.update newPos mulVal $ xs
  _ -> xs
  where applyOp op (a Seq.:<| b Seq.:<| _) = op <$> (Seq.lookup a xs) <*> (Seq.lookup b xs)
        sumVal = fromJust . applyOp (+) $ subseq
        mulVal = fromJust . applyOp (*) $ subseq
        subseq = Seq.take 2 . Seq.drop (pos + 1) $ xs
        newPos = fromJust . Seq.lookup (pos + 3) $ xs

runProg :: (Int, Int) -> Seq Int -> Maybe Int
runProg (noun, verb) xs =
  let xs' = (Seq.update 2 verb . Seq.update 1 noun) xs
      res = intCode 0 xs'
  in Seq.lookup 0 res

main :: IO()
main = do
  input <- readFile "input.txt"
  let lst = Seq.fromList . map (read::String->Int) . splitOn "," $ input
  putStrLn . show . fromJust . runProg (12, 2) $ lst
  let (noun, verb) = fromJust . listToMaybe $ [x | x <- liftA2 (,) [0..99] [0..99]
                                                 , Just 19690720 <- [runProg x lst]]
  putStrLn . show $ ((100 * noun) + verb)
