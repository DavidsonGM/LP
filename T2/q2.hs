import ModeloDados

{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um
medicamento e de um estoque de medicamentos, retorna um novo estoque de
medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário,
deve se retornar Just v, onde v é o novo estoque.

Medicamento = String
EstoqueMedicamentos = [(Medicamento, Quantidade)]

-}

-- temEmEstoque :: Medicamento -> EstoqueMedicamentos -> Bool
-- temEmEstoque _ [] = False
-- temEmEstoque med ((name, _):es)
--     | med == name = True
--     | otherwise = temEmEstoque med es

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento _ [] = Nothing
tomarMedicamento med (h_estoque:t_estoque)
    | med == fst h_estoque = Just ((med, (snd h_estoque) - 1):t_estoque)
    | otherwise = tomarMedicamento med t_estoque