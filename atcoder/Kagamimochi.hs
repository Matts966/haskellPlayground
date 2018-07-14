import Data.List
import Control.Monad
main :: IO()
main = do
  n <- readLn
  mochis <- replicateM n readLn :: IO [Int]
  print $ length . nub $ sort mochis
