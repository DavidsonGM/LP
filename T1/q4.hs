howMany :: String -> [String] -> Int
howMany _ [] = 0
howMany s (l:ls)
    | s == l = 1 + howMany s ls
    | otherwise = howMany s ls

histograma :: [String] -> [(String, Int)]
histograma [] = []
histograma l = [(a, howMany a l)| a <- l]
