import Control.Monad (replicateM)
import Data.List (sort)

-- ABC085B - Kagami Mochi
-- https://atcoder.jp/contests/abs/tasks/abc085_b

main :: IO ()
main =
  do
    n <- readLn :: IO Int
    d <- replicateM n readLn :: IO [Int]
    putStrLn $ show $ solve d

solve :: [Int] -> Int
solve d = length $ (uniq . sort) d

uniq :: [Int] -> [Int]
uniq (a : b : rest)
  | a == b = uniq (b : rest)
  | otherwise = a : (uniq (b : rest))
uniq [] = []
uniq [a] = [a]
