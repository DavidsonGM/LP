import ModeloDados

{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que,
a partir de um medicamento, uma quantidade e um estoque inicial de 
medicamentos, retorne um novo estoque de medicamentos contendo o 
medicamento adicionado da referida quantidade. Se o medicamento já 
existir na lista de medicamentos, então a sua quantidade deve ser 
atualizada no novo estoque. Caso o remédio ainda não exista no
estoque, o novo estoque a ser retornado deve ter o remédio e sua
quantidade como cabeça.

Medicamento = String
Quantidade = Int
EstoqueMedicamentos = [(Medicamento, Quantidade)]

-}

temEmEstoque :: Medicamento -> EstoqueMedicamentos -> Bool
temEmEstoque _ [] = False
temEmEstoque med ((name, _):es)
    | med == name = True
    | otherwise = temEmEstoque med es


comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento [] _ estoque = estoque
comprarMedicamento med qtd [] = [(med,qtd)] 
comprarMedicamento med qtd estoque
    | temEmEstoque med estoque == False = (med, qtd):estoque
    | med == fst(head estoque) = (fst(head estoque), snd(head estoque) + qtd):tail(estoque)
    | otherwise = (head estoque):(comprarMedicamento med qtd (tail estoque))
