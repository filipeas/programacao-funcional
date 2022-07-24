{- pagina 71 -}
{-
1)
-}
todosQuatroIguais :: Int -> Int -> Int -> Int -> Bool
todosQuatroIguais a b c d
    |(a == b) && (a == c) && (a == d) = True
    |otherwise = False

{-
2)
-}
todosIguais :: Int -> Int -> Int -> Bool
todosIguais n m p = (n == m) && (m == p)
todosQuatroIguais2 :: Int -> Int -> Int -> Int -> Bool
todosQuatroIguais2 a b c d
    |todosIguais a b c && todosIguais b c d = True
    |otherwise = False

{-
3)
o que há de errado:
- faltou a definicao da funcao
- a funcao so retorna true se (n m p) forem exatamente diferentes
- a funcao da true se n e p forem iguais
-}
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes n m p = ((n /= m) && (m /= p))

{-
4)
resposta:
- sim, pois foi criado uma funcao chamado avalia, que avalia se as funcoes
  todosIguais e teste dão o mesmo resultado. pela analise, a estrutura da
  função teste sempre condiz com os resultados da função todosIguais.
-}
teste :: Int -> Int -> Int -> Bool
teste n m p = ((n + m + p) == 3 * p)
avalia :: Int -> Int -> Int -> Bool
avalia n m p
    |teste n m p == todosIguais n m p = True
    |otherwise = False

{-
5)
-}
quantosIguais :: Int -> Int -> Int -> Int
quantosIguais a b c
    |(a == b) && (b == c) = 3
    |(a == b) || (b == c) || (a == c) = 2
    |otherwise = 0