{- pagina 41 -}
{-
4)
-}
potencia :: Int -> Int -> Int
potencia x n
    |n == 0 = 1
    |otherwise = x * potencia x (n - 1)

{-
5)
-}
exponenciacao :: Int -> Int -> Int
exponenciacao x n = potencia x n

{-
6)
-}
sucessor :: Int -> Int
sucessor n = n + 1
predecessor :: Int -> Int
predecessor n = n - 1
adicao :: Int -> Int -> Int
adicao a b
    |b == 0 = a
    |otherwise = adicao (sucessor a) (predecessor b)

{-
7)
nao calcula numeros negativos
-}
subtracao :: Int -> Int -> Int
subtracao a b
    |a < b && b < 0 = -1 * subtracao b a
    |b == 0 = a
    |a == 0 = b
    |otherwise = subtracao (predecessor a) (predecessor b)

{-
8)
-}
somaDivisores :: Int -> Int -> Int
somaDivisores n x
    |n == x = 1
    |(mod n x) /= 0 = somaDivisores n (x + 1)
    |(mod n x) == 0 = x + somaDivisores n (x + 1)
eperfeito :: Int -> Bool
eperfeito n
    |somaDivisores n 2 == n = True
    |somaDivisores n 2 /= n = False

{-
9)
-}
primo :: Int -> Int -> Bool
primo n i
    |n == 2 = True
    |n < 2 = False
    |mod n i == 0 = False
    |(i * i) > n = True
    |otherwise = primo n (i + 1)
eprimo :: Int -> Bool
eprimo n = primo n 2