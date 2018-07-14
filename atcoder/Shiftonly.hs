import Control.Applicative

div2Times :: Integer -> [Integer] -> Integer
div2Times x xs
  | all ((== 0) . (`mod` 2)) xs = div2Times (x + 1) $ map (`div` 2) xs
  | otherwise = x

main :: IO()
main = do
  n <- getLine
  numbers <- map read . words <$> getLine
  putStrLn . show $ div2Times 0 numbers
