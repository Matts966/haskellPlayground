import Control.Applicative
calcPayWay :: Int -> Int -> Int -> Int -> [Int]
calcPayWay n10000 n5000 n1000 y
  | any (< 0) comb = impossible
  | csum == y = comb
  | otherwise = let c5000 = calcPayWay n10000 (n5000 + 1) (n1000 - 1) y in
    if c5000 == impossible then
      calcPayWay (n10000 + 1) n5000 (n1000 - 1) y
    else
      c5000
    where impossible = [-1, -1, -1]
          comb = [n10000, n5000, n1000]
          csum = (n10000 * 10 + n5000 * 5 + n1000) * 1000

proc :: Int -> Int -> Int -> Int -> [Int]
proc n10000 n5000 n1000 y
  | y `mod` 1000 /= 0 = impossible
  | maxY < y = impossible
  | otherwise = calcPayWay n10000 n5000 n1000 y
    where impossible = [-1, -1, -1]
          maxY = 10000 * n1000

main :: IO()
main = do
  [n, y] <- map read . words <$> getLine
  let comb = proc 0 0 n y
  putStrLn . unwords $ map show comb
