{--
1) 
A quantidade de nodos externos em uma arv.bin.compl de altura d = 2^{d}.
Logo existem 2^5 arvores de tamanho 5.

abaixo foi implementado 2 funcoes diferentes que calculam a altura da arvore. A arvore pode ter N nós.
--}
data Tree = Leaf | Node Tree Int Tree
  deriving Show

depth :: Tree -> Int
depth Leaf = 0
depth (Node l _ r) = 1 + max (depth l) (depth r)

t :: Tree
t = Node (Node (Node Leaf 0 Leaf) 1 (Node (Node Leaf 0 Leaf) 2 Leaf)) 3 (Node (Node Leaf 4 Leaf) 5 (Node (Node Leaf 6 Leaf) 7 Leaf))

data ArvBin t = Folha t | No (ArvBin t) (ArvBin t) deriving (Eq, Ord, Show)
tamArvBin :: ArvBin t -> Int
tamArvBin (Folha a) = 1
tamArvBin (No ae ad) = tamArvBin ae + tamArvBin ad

{--
2)
Prova por indução:
N = 0 --> árvore vazia com apenas um nodo externo
N > 0 --> a raiz de uma árvore binária tem na
 subarv. esq: k nodos internos, 0 <= k <= N-1
 subarv. dir: N-k-1 nodos internos
Por hip.ind. a subarv. esq tem k+1 nodos externos e a subarv. dir N-k-1+1 nodos externos.
Assim, a árvore tem (k+1) + (N-k-1+1) = N+1 nodos externos.
Árvore binária completa de altura d é uma árvore binária na qual todos os nodos externos estão no
nível d.
--}

{--
3)
length (subarvores xt) e tamanho xt tem relação pois na primeira é calculado a altura da arvore e na segunda basta verificar o tamanho do vetor, pois isso reflete exatamente no length da árvore.
--}

{--
4)
Para cada nó é guardado a informação de formação da subarvore a qual ele pertence
--}