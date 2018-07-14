import Data.Char
main :: IO()
main = do
  ln <- getLine
  putStrLn . show . sum $ map digitToInt ln
