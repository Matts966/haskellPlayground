import Control.Monad

diffrentChars :: [[Char]]
diffrentChars = replicateM 8 ['a' .. 'z']

getSameHashStringFromDifferentChars :: [Char] -> [Char]
getSameHashStringFromDifferentChars [] = []
getSameHashStringFromDifferentChars (x:xs) = x : 'a' : getSameHashStringFromDifferentChars xs

main :: IO ()
main = do
  print (take 100 (map getSameHashStringFromDifferentChars diffrentChars))
