-- Letra a
sumArray :: Num u => [u] -> [u] -> [u]
sumArray [] _ = []
sumArray _ [] = []
sumArray (a:as) (b:bs) = (a + b) : sumArray as bs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a:as) (b:bs) = (f a b) : myZipWith f as bs

somaMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
somaMatricial a b = myZipWith sumArray a b

-- Letra b
matrizTransposta :: Num u => [[u]] -> [[u]]
matrizTransposta [] = []
matrizTransposta m = [a| (a:_) <- m ] : matrizTransposta [as| (_:as) <- m, (length as) > 0]

-- Letra c
multiplicaSoma :: Num u => [u] -> [u] -> u
multiplicaSoma [] _ = 0
multiplicaSoma _ [] = 0
multiplicaSoma (l:ls) (c:cs) = c * l + multiplicaSoma ls cs

multiplicacaoMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
multiplicacaoMatricial m1 m2 = [[multiplicaSoma m n|n <- (matrizTransposta m2)] | m <- m1]
