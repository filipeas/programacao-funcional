{-
1) O programa imprime a seguinte tabela:
__________
|| Semana | Vendas ||
______________
         0        12
         1        14
         2        15
         3        18
||     Total|        59||
| Media |   14.75||
-}

{-
2) 
enumeração: função vendas
caso: função totaldeVendas
composição: função imprimeSemanaJ
recursão: função imprimeespacos
intencionalidade: função totaldeVendas
-}

{-
3) O programa imprime a soma de dois elementos caso o primeiro elemento da tupla seja 1. Se for 2, realiza a subtração.
-}

{-
4) O programa lê a altura e sexo do usuário. Verifica se é F ou f e realiza um calculo, caso contrário, verifica se é M ou m e realiza outro calculo. Se não for nenhum deles, imprime sexo inválido.
-}

{-
5)
-}
calculaMedia :: Float -> Float -> Float -> String
calculaMedia a b c
    |((a + b + c) / 3) < 3 = "reprovado"
    |((a + b + c) / 3) >= 3 && ((a + b + c) / 3) < 7 = "exame final"
    |((a + b + c) / 3) >= 7 = "aprovado"