-- ABC083B - Some Sums
-- https://atcoder.jp/contests/abs/tasks/abc083_b

main :: IO ()
main = do
  [n, a, b] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ show $ solve n a b

solve :: Int -> Int -> Int -> Int
solve n a b = sum sums
  where
    sums = [x | x <- [1 .. n], (\l -> (a <= l) && (l <= b)) (sumDigits x)]

sumDigits :: Int -> Int
sumDigits n
  | n `div` 10 == 0 = mod n 10
  | otherwise = (mod n 10) + (sumDigits $ n `div` 10)
