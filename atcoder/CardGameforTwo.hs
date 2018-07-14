import Control.Applicative
import Data.List

cardDiff :: [Int] -> Int
cardDiff = abs . foldl (\acc x -> -1 * (acc + x)) 0 . sort

main :: IO()
main = do
  _ <- getLine
  cards <- map read . words <$> getLine
  print $ cardDiff cards
