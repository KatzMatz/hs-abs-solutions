import Data.List (sort)

{--
 - ABC088B - Card Game for Two
 - https://atcoder.jp/contests/abs/tasks/abc088_b
 - --}

main :: IO ()
main = do
  _ <- readLn :: IO Int
  a <- map read . words <$> getLine :: IO [Int]
  putStrLn $ show $ solve a

solve :: [Int] -> Int
solve a = alice - bob
  where
    (alice, bob) = takeCard ((reverse . sort) a) (0, 0) True

takeCard :: [Int] -> (Int, Int) -> Bool -> (Int, Int)
takeCard [] (alice, bob) _ = (alice, bob)
takeCard (x : xs) (alice, bob) True = takeCard xs (alice + x, bob) False
takeCard (x : xs) (alice, bob) False = takeCard xs (alice, bob + x) True
