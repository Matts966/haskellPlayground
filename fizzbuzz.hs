fizzbuzz :: Integer -> [Char]
fizzbuzz 0 = ""
fizzbuzz n
  | mod n 15 == 0 = fizzbuzz(n - 1) ++ "fizzbuzz"
  | mod n  3 == 0 = fizzbuzz(n - 1) ++ "fizz"
  | mod n  5 == 0 = fizzbuzz(n - 1) ++ "buzz"
fizzbuzz n = fizzbuzz(n - 1) ++ show n

fizzbuzzList :: [Integer] -> [Char]
fizzbuzzList [] = ""
fizzbuzzList l
  | last l `mod` 15 == 0 = fizzbuzzList(init l) ++ "fizzbuzz"
  | last l `mod`  3 == 0 = fizzbuzzList(init l) ++ "fizz"
  | last l `mod`  5 == 0 = fizzbuzzList(init l) ++ "buzz"
fizzbuzzList l = fizzbuzzList(init l) ++ show (last l)  

main = do
  print (fizzbuzz 5)
  print (fizzbuzzList [1..5])
