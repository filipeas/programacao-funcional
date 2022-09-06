{--
1)
--}
data Tempo = Quente | Frio deriving(Show)
data Estacao = Verao | Outono | Inverno | Primavera deriving(Show)

tempo :: Estacao -> Tempo
tempo Verao = Quente
tempo Outono = Frio
tempo Inverno = Quente
tempo Primavera = Quente

{--
2)
--}
data Meses = Jan | Fev | Mar | Abr | Mai | Jun | Jul | Ago | Set | Out | Nov | Dez deriving(Eq, Show, Enum)
associacao :: Meses -> Estacao
associacao Jan = Inverno
associacao Fev = Inverno
associacao Mar = Inverno
associacao Abr = Inverno
associacao Mai = Inverno
associacao Jun = Verao
associacao Jul = Verao
associacao Ago = Verao
associacao Set = Verao
associacao Out = Verao
associacao Nov = Verao
associacao Dez = Verao

{--
3)
--}
data Forma = Circulo Float Float Float | Retangulo Float Float Float Float | Triangulo Float Float Float deriving Show
perimetro :: Forma -> Float
perimetro (Circulo _ _ r) = 2 * pi * r
perimetro (Retangulo x1 y1 x2 y2) = 2 * (y2 + x2)

{--
4)
--}
perimetro (Triangulo a b c) = a + b + c