maior4 :: Int -> Int -> Int -> Int -> Int
maior4 n1 n2 n3 n4
    | n1 >= n2 && n1 >= n3 && n1 >= n4 = n1
    | n2 >= n1 && n2 >= n3 && n2 >= n4 = n2
    | n3 >= n1 && n3 >= n2 && n3 >= n4 = n3
    | otherwise = n4
    