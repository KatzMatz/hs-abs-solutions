-- ABC081A - Placing Marbles
-- https://atcoder.jp/contests/abs/tasks/abc081_a

main :: IO ()
main = do
  s <- getLine :: IO String
  putStrLn $ show $ countElem s '1' 0

countElem :: String -> Char -> Int -> Int
countElem [] _ ans = ans
countElem (x : xs) elem ans
  | x == elem = countElem xs elem (ans + 1)
  | otherwise = countElem xs elem ans
