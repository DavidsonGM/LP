import ModeloDados
import Data.List(sort)

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

Um receituário é válido se, e somente se, 
todo os medicamentos são distintos e estão ordenados lexicograficamente e,
para cada medicamento, seus horários também estão ordenados e são distintos.

Inversamente, um plano de medicamentos é válido se, e somente se,
todos seus horários também estão ordenados e são distintos,
e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

Defina as funções "receituarioValido" e "planoValido"
que verifiquem as propriedades acima e cujos tipos são dados abaixo:

Medicamento = String
Horario = Int
Prescricao = (Medicamento, [Horario])

Receituario = [Prescricao]
PlanoMedicamento = [(Horario, [Medicamento])]

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

distinctAndOrdered :: Eq t => Eq u => Ord t => Ord u => [(t, [u])] -> Bool
distinctAndOrdered [] = True
distinctAndOrdered r 
    | (distinct [m| (m, _) <- r]) && (sort [m| (m, _) <- r]) == [m| (m, _) <- r]
    && (distinct (snd(head r))) && (sort(snd(head r)) == snd(head r)) = distinctAndOrdered (tail r)
    | otherwise = False

receituarioValido :: Receituario -> Bool
receituarioValido r = distinctAndOrdered r

planoValido :: PlanoMedicamento -> Bool
planoValido p = distinctAndOrdered p
