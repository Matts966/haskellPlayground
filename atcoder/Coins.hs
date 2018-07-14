howPay :: Integer -> Integer -> Integer -> Integer -> Integer
howPay c500 c100 c50 target
  | any (< 0) [c500, c100, c50] = 0
  | target == 0 = 1
  | target <= 0 = 0
  | target >= 500 = howPay (c500 - 1) c100 c50 (target - 500)
    + howPay 0 (c100 - 1) c50 (target - 100)
    + howPay 0 0 (c50 - 1) (target - 50)
  | target >= 100 = howPay 0 (c100 - 1) c50 (target - 100)
    + howPay 0 0 (c50 - 1) (target - 50)
  | target >= 50 = howPay 0 0 (c50 - 1) (target - 50)
  | otherwise = 0

main :: IO()
main = do
  c500 <- readLn
  c100 <- readLn
  c50 <- readLn
  target <- readLn
  print $  howPay c500 c100 c50 target
