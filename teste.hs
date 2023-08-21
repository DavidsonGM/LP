myFunction :: Integer -> Either String Integer
myFunction n
    | n > 0 = Right (n - 1)
    | otherwise = Left "Número negativo"

otherFunc :: Either String Integer -> Either String Integer
otherFunc (Right n) = Right (n - 1)
otherFunc (Left _) = Left "Deu bom"

getEither :: Either a a -> c
getEither (Right n) = n
getRight (Left n) = n
