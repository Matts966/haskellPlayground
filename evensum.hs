evenSum [] = 0
evenSum (x : xs) = (if mod x 2 == 0 then x else 0) + evenSum xs
