splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = foldl (\acc e -> if e == x
  then acc ++ [[]]
  else init acc ++ [last acc ++ [e]])
  [[]]

readInt :: String -> Integer
readInt xs = read xs :: Integer

main :: IO()
main = do
  a <- getLine
  bc <- getLine
  s <- getLine
  putStr $ show . sum . map readInt $ a : splitOn ' ' bc
  putStr " "
  putStrLn s
