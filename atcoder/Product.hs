import Control.Applicative

main :: IO()
main = do
  [a, b] <- map read . words <$> getLine
  if odd $ a * b then putStrLn "Odd"
    else putStrLn "Even"
