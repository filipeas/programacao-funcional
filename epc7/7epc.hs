{-- 
1) 
--}
index = [0..3]
test = ['A'..'F']
concatchar = test ++['-'] ++ [test !! y | y <- index]

n = 100
testHipo = [1.. (n + 1)]
hipotenusa = [(x,y,z) | x <- testHipo, y <- testHipo, z <- testHipo, x^2 + y^2 == z^2, z <= n]

type Matriz = [[Integer]]
testMatriz= [[1,2,3], [4,5,-10],[7,8,9]]

verificarTamanho :: Matriz -> Int -> Bool
verificarTamanho [] t = True
verificarTamanho (x:xs) t = (length x == t) && verificarTamanho xs t

{-- 
2) 
--}
calcPrincipal :: Matriz -> Int -> Integer
calcPrincipal [] _ = 1
calcPrincipal (x:xs) ac = x !! indice * calcPrincipal xs (indice + 1)
    where 
        indice
            | ac >= length x = ac - length x
            | otherwise = ac

{-- 
3) 
--}
determinante :: Matriz -> Integer
determinante m = if verificarTamanho m (length m) 
    then sum [calcPrincipal m y |y <- [0..(length m - 1)]] - sum [calcPrincipal (reverse m) y |y <- [0..(length m - 1)]] 
    else error "nao é uma matriz quadrada"

{-- 
4) 
--}
imparPar :: [Integer] -> [[Integer]]
imparPar lista = [[x | x <- lista, odd x], [x | x <- lista, even x]] 

{-- 
6) 
--}
sublistaQuestion :: [Integer] -> Int -> [[Integer]]
sublistaQuestion [] _ = []
sublistaQuestion lista n = take n lista : cont
    where
        cont = if (length (drop n lista) `div` n) >= 2 
            then sublistaQuestion (drop n lista) n 
            else (drop n lista) : sublistaQuestion [] n

matrizQuestionSete = [[2,3,4], [5,6,7], [8,9,10]]
matriz7ZF = [x | x <- matrizQuestionSete]

{-- 
7) 
--}
transposta :: Matriz -> Int -> [Integer]
transposta [] _ = []
transposta (x:xs) ind = x !! ind : transposta xs ind

matrizTransposta matriz = [transposta matriz x | x <- [0..(length matriz - 1)]]