-- ABC085C - Otoshidama
-- https://atcoder.jp/contests/abs/tasks/abc085_c

main :: IO ()
main = do
  [n, y] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ unwords $ map show $ solve n y

solve :: Int -> Int -> [Int]
solve n y = takeFromList comb
  where
    comb = [(a, b, c) | a <- [0 .. n], b <- [0 .. n - a], let c = n - (a + b), (howMuch a b c) == y]

howMuch :: Int -> Int -> Int -> Int
howMuch a b c = 10000 * a + 5000 * b + 1000 * c

takeFromList :: [(Int, Int, Int)] -> [Int]
takeFromList [] = [-1, -1, -1]
takeFromList ((a, b, c) : rest) = [a, b, c]
