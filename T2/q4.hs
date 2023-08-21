import ModeloDados
import Data.List(sort)

{-
   QUESTÃO 4  VALOR: 1,0 ponto

Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa
a demanda de todos os medicamentos por um dia a partir do receituario.
O retorno é do tipo EstoqueMedicamentos e deve ser
ordenado lexicograficamente pelo nome do medicamento.

Dica: Observe que o receituario lista cada remédio
e os horários em que ele deve ser tomado no dia.
Assim, a demanda de cada remédio já está latente no receituario,
bastando contar a quantidade de vezes que cada remédio é tomado.

Medicamento = String
Quantidade = Int
EstoqueMedicamentos = [(Medicamento, Quantidade)]
Horario = Int
Prescricao = (Medicamento, [Horario])
Receituario = [Prescricao]

-}

doses :: [Horario] -> Quantidade
doses [] = 0
doses (_:as) = 1 + doses as 

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos receituario = sort [(med, doses horarios) | (med, horarios) <- receituario]
