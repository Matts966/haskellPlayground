tellList :: (Show a) => [a] -> String
tellList [] = "This is an empty list."
tellList (e:[]) = "This list has one element, " ++ show e
tellList (e:e2:[]) = "This list has two elements, " ++ show e ++ " and " ++ show e2
tellList (e:e2:_)  = "This list has a lot of elements. The first two elements are " ++ show e ++ " and "  ++ show e2 ++ "."

firstLetter :: String -> String
firstLetter "" = " Empty string, whoops!" 
firstLetter all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]

main :: IO()
main = do
  print (tellList [1 .. 100])
  print (firstLetter "Ok")
  
