import ModeloDados

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, 
a partir de um medicamento e de um estoque de medicamentos,
retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

Medicamento = String
Quantidade = Int
EstoqueMedicamentos = [(Medicamento, Quantidade)]

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento _ [] = 0
consultarMedicamento med (h_estoque:t_estoque)
    | med == fst h_estoque = snd h_estoque
    | otherwise = consultarMedicamento med t_estoque
