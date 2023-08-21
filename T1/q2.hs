converterNotaParaMencao :: Float -> String
converterNotaParaMencao n
    | n >= 9 && n <= 10 = "SS"
    | n >= 7 && n < 9 = "MS"
    | n >= 5 && n < 7 = "MM"
    | n >= 3 && n < 5 = "MI"
    | n > 0  && n < 3 = "II"
    | otherwise = "SR"
