-- ABC081A - Placing Marbles
-- https://atcoder.jp/contests/abs/tasks/abc081_a

main :: IO ()
main = do
  s <- getLine :: IO String
  putStrLn $ show $ length $ filter (== '1') s
