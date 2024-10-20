module Main (main) where

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ solve a b

solve :: Int -> Int -> String
solve a b
  | mod (a * b) 2 == 0 = "Even"
  | otherwise = "Odd"
