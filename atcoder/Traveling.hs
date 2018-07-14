import Control.Monad
import Control.Applicative

eachSub :: [Int] -> [Int] -> [Int]
eachSub [] [] = []
eachSub [] (_:_) = []
eachSub (_:_) [] = []
eachSub (x:xs) (y:ys) = (x - y) : eachSub xs ys

isPossibleTravel :: [[Int]] -> Bool
isPossibleTravel [] = True
isPossibleTravel (x:y:xys)
  | (h >= t) && ((h - t) `mod` 2 == 0) = True &&
    isPossibleTravel (eachSub y x : xys)
  | otherwise = False
  where h = head x
        t = sum $ tail x
isPossibleTravel [x]
  | (h >= t) && ((h - t) `mod` 2 == 0) = True
  | otherwise = False
  where h = head x
        t = sum $ tail x


main :: IO()
main = do
  n <- readLn
  timeAndPlaces <- replicateM n (map read . words <$> getLine) :: IO [[Int]]
  if isPossibleTravel timeAndPlaces then putStrLn "Yes"
    else putStrLn "No"
