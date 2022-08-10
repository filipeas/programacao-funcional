contaIndice _ _ [ ] = -1
contaIndice indice i (a:x)
    |i /= a = contaIndice (indice + 1) i x
    |otherwise = indice

index i (a:x) = contaIndice 0 i (a:x)