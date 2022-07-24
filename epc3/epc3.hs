{- pagina 88 -}
{-
1) Implementar a definição quantosIguaisValor
-}
quantosIguaisValor :: Int -> Int -> Int -> Int -> Int
quantosIguaisValor valor n m p = ehN + ehM + ehP
    where
        ehN = if n == valor then 1 else 0
        ehM = if m == valor then 1 else 0
        ehP = if p == valor then 1 else 0

{-
2) Implementar função que calcula bascara
- ax^2 + bx + c = 0
- delta = b^2 - 4*a*c
- se delta < 0, não há raizes
- se delta > 0, tem raizes
- se delta = 0, só tem 1 raiz
- -b +- sqrt(delta) / 2a
-}
bascara :: Float -> Float -> Float -> (Float, Float)
bascara a b c
    |delta < 0 = (-1, -1)
    |delta > 0 = raizes
    |delta == 0 = raizes
    where
        delta = b**2 - (4*a*c)
        raizes = ((-b + sqrt(delta)) / (2 * a), (-b - sqrt(delta)) / (2 * a))