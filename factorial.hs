factorial :: Integer -> Integer
factorial 0 = 1
factorial x = factorial (x - 1) * x

main = do
  print (factorial 100)
