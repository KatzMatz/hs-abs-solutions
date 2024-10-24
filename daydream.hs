import Data.List qualified as L
import Data.Maybe (listToMaybe, mapMaybe)

-- ABC049C - 白昼夢
-- https://atcoder.jp/contests/abs/tasks/arc065_a

main :: IO ()
main = do
  s <- getLine :: IO String
  putStrLn $
    if solve s
      then "YES"
      else "NO"

solve :: String -> Bool
solve s = (isMatch . reverse) s
  where
    targets = map reverse ["dream", "dreamer", "erase", "eraser"]
    isMatch [] = True
    isMatch ss = case listToMaybe $ mapMaybe (`L.stripPrefix` ss) targets of
      Nothing -> False
      Just hs -> isMatch hs
