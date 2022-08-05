import Data.Time
type Pessoa = String
type Livro = String
type BancodeDados = [(Pessoa, Livro)]
teste = [
    ("Paulo", "A Mente Nova do Rei"), 
    ("Ana", "O Segredo de Luiza"), 
    ("Helson","A Mente Nova do Rei"),
    ("Paulo", "O Pequeno Principe"), 
    ("Paulo", "Arte da guerra"), 
    ("Mauro", "O Capital"),
    ("Francisco", "O Auto da Compadecida")
    ]

{-
1) operações de consulta
-}
{- a) Uma funcao que informa os livros que uma determinada pessoa tomou emprestado -}
questao1A :: BancodeDados -> Pessoa -> [Livro]
questao1A [ ] _ = [ ]
questao1A ((possuidor, titulo) : resto) pessoa
    |possuidor == pessoa = titulo : questao1A resto pessoa
    |otherwise = questao1A resto pessoa

{- b) Uma funcao que informa todas as pessoas que tomaram emprestado um determinado livro. -}
questao1B :: BancodeDados -> Livro -> [Pessoa]
questao1B [ ] _ = [ ]
questao1B ((possuidor, titulo) : resto) livro
    |titulo == livro = possuidor : questao1B resto livro
    |otherwise = questao1B resto livro

{- c) Uma funcao que informa se um determinado livro esta ou nao emprestado. -}
questao1C :: BancodeDados -> Livro -> Bool
questao1C [ ] _ = False
questao1C ((possuidor, titulo) : resto) livro
    |titulo == livro = True
    |otherwise = False || questao1C resto livro

{- d) Uma funcao que informa a quantidade de livros que uma determinada pessoa tomou emprestado -}
questao1D :: BancodeDados -> Pessoa -> Int
questao1D [ ] _ = 0
questao1D ((possuidor, titulo) : resto) pessoa
    |possuidor == pessoa = 1 + questao1D resto pessoa
    |otherwise = questao1D resto pessoa

{-
2) operações de atualização
-}
{- a) Uma funcao que atualiza a base de dados, quando um livro é emprestado a alguem -}
questao2A :: BancodeDados -> Pessoa -> Livro -> BancodeDados
questao2A banco pessoa livro = (pessoa, livro) : banco

{- b) Uma funcao que atualiza a base de dados quando um livro é devolvido -}
questao2B :: BancodeDados -> Pessoa -> Livro -> BancodeDados
questao2B [ ] _ _ = error "Base de dados vazia"
questao2B ((possuidor, titulo) : resto) pessoa livro
    |possuidor == pessoa && titulo == livro = resto
    |otherwise = (possuidor, titulo) : questao2B resto pessoa livro

{-
3) operações de modificação
-}
{- a) exista um numero maximo de livros que uma pessoa possa tomar emprestado -}
questao3A :: BancodeDados -> Pessoa -> Livro -> BancodeDados
questao3A ((possuidor, titulo) : resto) pessoa livro
    |(questao1D ((possuidor, titulo) : resto) pessoa) >= 3 = error "Voce nao pode mais pedir livros pois o limite de emprestimos sao 3 livros por pessoa"
    |otherwise = questao2A ((possuidor, titulo) : resto) pessoa livro

{- b) exista uma lista de palavras-chave associadas a cada livro, de forma que cada livro possa ser encontrado atraves das palavras-chave a ele associadas -}
livros = [
    ("1", "A Mente Nova do Rei"),
    ("2", "O Segredo de Luiza"),
    ("3", "O Pequeno Principe"),
    ("4", "Arte da guerra"),
    ("5", "O Capital"),
    ("6", "O Auto da Compadecida"),
    ("7", "JAVA")
    ]

teste2 = [
    ("Paulo", "1"),
    ("Ana", "2"),
    ("Helson","1"),
    ("Paulo", "3"),
    ("Paulo", "4"),
    ("Mauro", "5"),
    ("Francisco", "6"),
    ("Filipe", "7")
    ]

questao3B :: BancodeDados -> BancodeDados -> BancodeDados
questao3B [ ] [ ] = error "bancos de dados vazios"
questao3B ((codigo, livro) : livros) ((possuidor, codTitulo) : resto)
    |codigo == codTitulo = (possuidor, livro) : questao3B livros resto
    |otherwise = questao3B ((codigo, livro) : livros) resto

{- c) existam datas associadas aos emprestimos, para poder detectar os livros com datas de emprestimos vencidas -}


type Emprestimo = Integer
type BancodeDados2 = [(Pessoa, Livro, Emprestimo)]

teste3 = [
    ("Paulo", "A Mente Nova do Rei", 1), 
    ("Ana", "O Segredo de Luiza", 2), 
    ("Helson","A Mente Nova do Rei", 3),
    ("Paulo", "O Pequeno Principe", 4), 
    ("Paulo", "Arte da guerra", 5), 
    ("Mauro", "O Capital", 6),
    ("Francisco", "O Auto da Compadecida", 7)
    ]

questao3C :: BancodeDados2 -> Pessoa -> Livro -> Integer -> Bool
questao3C [ ] _ _ _ = False
questao3C ((possuidor, titulo, emprestimo) : resto) pessoa livro dataEmpr
    |possuidor == pessoa && titulo == livro && emprestimo < dataEmpr = True
    |otherwise = questao3C resto pessoa livro dataEmpr