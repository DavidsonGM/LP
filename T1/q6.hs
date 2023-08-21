import Data.List (sortOn)

getMedia :: Float -> Float -> Float
getMedia n1 n2 = (n1 + n2)/2

aprovadosOrdemDeMedia :: [(String, Float, Float)] -> [(String, Float)]
aprovadosOrdemDeMedia student = sortOn snd [(name, getMedia n1 n2)| (name, n1, n2) <- student, (getMedia n1 n2) >= 5 ]
