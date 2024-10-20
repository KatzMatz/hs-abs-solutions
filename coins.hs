-- ABC087B - Coins
-- https://atcoder.jp/contests/abs/tasks/abc087_b

main :: IO ()
main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  c <- readLn :: IO Int
  x <- readLn :: IO Int
  putStrLn $ show $ solve a b c x

solve :: Int -> Int -> Int -> Int -> Int
solve a b c x = length ans
  where
    ans = [(aa, bb, cc) | aa <- [0 .. a], bb <- [0 .. b], cc <- [0 .. c], (howMuch aa bb cc) == x]

howMuch :: Int -> Int -> Int -> Int
howMuch a b c = 500 * a + 100 * b + 50 * c
