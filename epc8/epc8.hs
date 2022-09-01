import Text.Printf
import Control.Exception
import System.CPUTime

{-- 
para executar:
1) ghci epc8.hs
2) main
Opcional: Quando executar esse arquivo, no terminal do ghci execute: 
:set +s

Assim ele sempre vai dizer o tempo de execução do que for executado.
--}

exec1 = [1,3,6,7,2,9,12,56,78]
exec2 = [7,2,78,1,6,3,9,12,56]
exec3 = [78,56,12,9,2,7,6,3,1]

{--
selection sort

tempo de execução em C:
execução 1: 0.00001
execução 2: 0.00000
execução 3: 0.00000
--}
selecao:: (Ord a) => [a]->[a]
selecao [] = []
selecao xs = [x] ++ selecao (remove x xs) where x = minimo xs

remove:: (Ord a) => a->[a]->[a]
remove a [] = []
remove a (x:xs)
    | a == x = xs
    | otherwise = x:(remove a xs)

minimo:: (Ord a) => [a]->a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
    | x <= (minimo xs) = x
    | otherwise = minimo xs

{--
insertion sort

tempo de execução em C:
execução 1: 0.00003
execução 2: 0.00001
execução 3: 0.00001
--}
insercao:: (Ord a) => [a]->[a]
insercao [] = []
insercao (x:xs) = insereOrd x (insercao xs)

insereOrd:: (Ord a) => a->[a]->[a]
insereOrd x [] = [x]
insereOrd x (y:ys)
    | x <= y = (x:y:ys)
    | otherwise = y: (insereOrd x ys)

{--
bubble sort

tempo de execução em C:
execução 1: 0.00007
execução 2: 0.00004
execução 3: 0.00004
--}
bolha [] = []
bolha lista = bolhaOrd lista (length lista)

bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (troca lista) (n - 1)

troca [x] = [x]
troca (x:y:zs)
    | x > y = y : troca (x:zs)
    | otherwise = x : troca (y:zs)

{--
quick sort

tempo de execução em C:
execução 1: 0.00001
execução 2: 0.00001
execução 3: 0.00002
--}
quicksort::(Ord a) => [a]->[a]
quicksort [] = []
quicksort (s:xs) = quicksort [x | x <- xs, x < s] ++ [s] ++ quicksort [x | x <- xs, x >= s]

{--
quick sort

tempo de execução em C:
execução 1: 0.00001
execução 2: 0.00001
execução 3: 0.00001
--}
mergesort :: Ord a => [a] -> [a] -> [a]
mergesort xs [] = xs
mergesort [] ys = ys
mergesort (x:xs) (y:ys) | x <= y    = x:mergesort xs (y:ys)
                    | otherwise = y:mergesort (x:xs) ys

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.5f sec\n" (diff :: Double)
    return v

main = do
    putStrLn "Iniciando..."

    putStrLn "Selection Sort"
    time $ selecao exec1 `seq` return ()
    time $ selecao exec2 `seq` return ()
    time $ selecao exec3 `seq` return ()

    putStrLn "Insertion Sort"
    time $ insercao exec1 `seq` return ()
    time $ insercao exec2 `seq` return ()
    time $ insercao exec3 `seq` return ()

    putStrLn "Bubble Sort"
    time $ bolha exec1 `seq` return ()
    time $ bolha exec2 `seq` return ()
    time $ bolha exec3 `seq` return ()

    putStrLn "Quick Sort"
    time $ quicksort exec1 `seq` return ()
    time $ quicksort exec2 `seq` return ()
    time $ quicksort exec3 `seq` return ()

    putStrLn "Merge Sort"
    time $ mergesort exec1 `seq` return ()
    time $ mergesort exec2 `seq` return ()
    time $ mergesort exec3 `seq` return ()
    putStrLn "Pronto."