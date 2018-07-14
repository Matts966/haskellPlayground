import Control.Applicative

main :: IO ()
main = do
    a <- readLn
    -- <$>はIOモナドへの関手
    [b, c] <- map read . words <$> getLine
    s <- getLine
    putStrLn $ show (a + b + c) ++ " " ++ s
