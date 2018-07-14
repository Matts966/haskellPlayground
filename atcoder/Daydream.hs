import Data.List

recursivePrefixChecker :: String -> Bool
recursivePrefixChecker str
  | dreamP && dreamerP = recursivePrefixChecker (drop dreamL str) ||
    recursivePrefixChecker (drop dreamerL str)
  | dreamP = recursivePrefixChecker $ drop dreamL str
  | dreamerP = recursivePrefixChecker $ drop dreamerL str
  | eraseP && eraserP = recursivePrefixChecker (drop eraseL str) ||
    recursivePrefixChecker (drop eraserL str)
  | eraseP = recursivePrefixChecker $ drop eraseL str
  | eraserP = recursivePrefixChecker $ drop eraserL str
  | str == "" = True
  | otherwise = False
    where
      (dreamP, dreamL) = ("dream" `isPrefixOf` str, length "dream")
      (dreamerP, dreamerL) = ("dreamer" `isPrefixOf` str, length "dreamer")
      (eraseP, eraseL) = ("erase" `isPrefixOf` str, length "erase")
      (eraserP, eraserL) = ("eraser" `isPrefixOf` str, length "eraser")

main :: IO()
main = do
  ln <- getLine
  if recursivePrefixChecker ln then putStrLn "YES"
    else putStrLn "NO"
