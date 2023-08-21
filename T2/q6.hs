import ModeloDados
import Data.List(sort)

{-

   QUESTÃO 6  VALOR: 1,0 ponto,

Um plantão é válido se, e somente se,
todas as seguintes condições são satisfeitas:

1. Os horários da lista são distintos e estão em ordem crescente;
2. Não há, em um mesmo horário, ocorrência de compra e medicagem
de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
3. Para cada horário, as ocorrências de
Medicar estão ordenadas lexicograficamente.

Defina a função "plantaoValido" que verifica
as propriedades acima e cujo tipo é dado abaixo:

Horario = Int
Medicamento = String
Quantidade = Int

Cuidado = Comprar Medicamento Quantidade | Medicar Medicamento

Plantao = [(Horario, [Cuidado])]

-}

howMany :: Eq t => t -> [t] -> Int
howMany _ [] = 0
howMany s (l:ls)
    | s == l = 1 + howMany s ls
    | otherwise = howMany s ls

distinct :: Eq t => [t] -> Bool
distinct [] = True
distinct (a:as)
    | (howMany a as) > 0 = False
    | otherwise = distinct as

-- boughtSameAsSold :: [Cuidado] -> Bool
-- boughtSameAsSold [((Comprar med _):cs)] = 
--     | med == 

distinctAndOrdered :: Eq t => Eq u => Ord t => Ord u => [(t, [u])] -> Bool
distinctAndOrdered [] = True
distinctAndOrdered r 
    | (distinct [m| (m, _) <- r]) && (sort [m| (m, _) <- r]) == [m| (m, _) <- r]
    && (distinct (snd(head r))) && (sort(snd(head r)) == snd(head r)) = distinctAndOrdered (tail r)
    | otherwise = False

plantaoValido :: Plantao -> Bool
plantaoValido p = (distinct [m| (m, _) <- p]) && (sort [m| (m, _) <- p]) == [m| (m, _) <- p]
