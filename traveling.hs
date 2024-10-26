import Control.Monad (replicateM)

main :: IO ()
main = do
  n <- readLn :: IO Int
  points <- replicateM n $ toTupple . map read . words <$> getLine :: IO [(Int, Int, Int)]
  let withStartPoint = (0, 0, 0) : points
  putStrLn $ if solve withStartPoint then "Yes" else "No"

toTupple :: [Int] -> (Int, Int, Int)
toTupple [a, b, c] = (a, b, c)
toTupple parsed = error "input error"

solve :: [(Int, Int, Int)] -> Bool
solve points = and $ zipWith isPossible points (tail points)

isPossible :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
isPossible (t1, x1, y1) (t2, x2, y2) =
  let dx = abs (x1 - x2)
      dy = abs (y1 - y2)
      dt = t2 - t1
      leftTime = dt - (dx + dy)
   in leftTime >= 0 && even leftTime
