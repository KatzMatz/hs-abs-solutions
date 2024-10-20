import Data.List (sort)

-- ABC081B - Shift only
-- https://atcoder.jp/contests/abs/tasks/abc081_b

main :: IO ()
main = do
  _ <- readLn :: IO Int
  a <- map read . words <$> getLine :: IO [Int]
  putStrLn $ show $ solve a

solve :: [Int] -> Int
solve a = minimum counts
  where
    counts = map countDevide2 a

countDevide2 :: Int -> Int
countDevide2 n
  | mod n 2 == 0 = 1 + (countDevide2 $ n `div` 2)
  | otherwise = 0
