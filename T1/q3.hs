isDecrescente :: [Int] -> Bool
isDecrescente [] = True
isDecrescente (n:ns) 
    | ns /= [] && n <= head ns = False
    | otherwise = isDecrescente ns
