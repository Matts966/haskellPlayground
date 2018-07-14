import Control.Applicative
calcPayWay :: Int -> Int -> [[Int]]
calcPayWay n y = [[a, b, c] | a <- [0..n], b <- [0..(n-a)], let c = n - a - b,
  a * 10000 + b * 5000 + c * 1000 == y]

proc :: Int -> Int  -> [Int]
proc n y
  | y `mod` 1000 /= 0 = impossible
  | maxY < y = impossible
  | otherwise = let a = calcPayWay n y in if null a then impossible else head a
    where impossible = [-1, -1, -1]
          maxY = 10000 * n

main :: IO()
main = do
  [n, y] <- map read . words <$> getLine
  let comb = proc n y
  putStrLn . unwords $ map show comb
