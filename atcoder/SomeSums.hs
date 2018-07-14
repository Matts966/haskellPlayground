import Data.Char
import Control.Applicative

cond :: Int -> Int -> Int -> Bool
cond a b x = a <= digSum && digSum <= b
  where digSum = sum . map digitToInt $ show x

main :: IO()
main = do
  [n, a, b] <- map read . words <$> getLine
  print . sum $ filter (cond a b)  [1..n]
