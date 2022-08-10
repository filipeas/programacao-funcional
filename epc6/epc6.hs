{-
1) De definicoes de funcoes que tome uma lista de inteiros l
-}
{- a) retorne a lista dos quadrados dos elementos de l -}
primeiraA [ ] = [ ]
primeiraA (a:x) = a * a : primeiraA x

{- b) retorne a soma dos quadrados dos elementos de l -}
primeiraB [ ] = 0
primeiraB (a:x) = (a * a) + primeiraB x

{- c) verifique se todos os elementos da lista sao, ou nao, positivos -}
primeiraC [ ] = True
primeiraC (a:x)
    |a < 0 = False
    |otherwise = True && primeiraC x

{-
2) Defina funcoes que
-}

{- a) dê o valor mınimo de uma funcao f aplicada a uma lista de 0 a n -}
segundaA [a] = a
segundaA (a:x) = if (a < (segundaA x)) then a else (segundaA x)

{- b) teste se os valores de f sobre as entradas 0 a n sao todas iguais -}
segundaB [] = True
segundaB [_] = True
segundaB (x:xs) = x == (head xs) && segundaB xs

{- c) teste se todos os valores de f aplicada ás entradas de 0 a n sao maiores ou iguais a zero -}
segundaC [] = True
segundaC [_] = True
segundaC (a:x) = a >= 0 && segundaC x

{- d) teste se os valores f 0, f 1 ate f n estao em ordem crescente -}
segundaD [] = True
segundaD [_] = True
segundaD (a:x) = a < (head x) && segundaD x