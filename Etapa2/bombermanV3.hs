module BombermanV3 where

{-
  Autores: Kemuel Santos Peres - 11811BCC035
           Paulo Vitor Costa Silva - 12011BCC045
           Pedro Lima Guarnieri - 11821BCC038
-}

{-
   Estruturas de dados para representação dos elementos do jogo usando listas
   Uso de tipos primitivos para representação do estado e dos elementos do jogo.
        Uso de type
   Funções de manipulação das estruturas
-}

{-

    Tabuleiro é uma tupla com 8 Linha.
    Linha é uma tupla com 8 Célula.
    Célula é uma pilha com 4 Item

-}

--Fazer um random: presente = presente_patins || presente_arremesso

type Player  = (Int, Coordenadas, Capacidades)  -- Definição de um jogador
data Elem = Patins | Bomba | Arremesso  -- Elementos para a capacidade
                deriving (Eq,Show)
data Item = Grama | Pedra | Presente Elem| Parede | Jogador Int  -- Itens possíveis para uma célula
            -- Jogador_1 | Jogador_2 | Jogador_3 | Jogador_4
            deriving (Eq,Show)

type Célula = [Item]
type Linha = [Célula]
type Tabuleiro = [Linha]

{-
Algumas regras devem ser respeitadas pela pilha, onde "sobre" quer dizer imediatamente subsequente acima na pilha.

    -> uma pilha vazia é um buraco no tabuleiro
    -> grama só pode estar na base da pilha
    -> presente só pode estar sobre grama
    -> parede só pode estar na base da pilha, sobre grama ou sobre presente
    -> pedra só pode estar na base da pilha
    -> bomba só pode estar sobre grama
    -> jogador só pode estar sobre grama
-}

regras :: Célula -> Bool  -- Verifica as regras acima citadas
regras r
  | null r = True  -- Célula vazia
  | elem Grama r && (head r) /= Grama = False
  | elem Pedra r && length r /= 1 = False
  | elem (Jogador 1) r && (r !! 1) /= (Jogador 1) = False
  | elem (Jogador 2) r && (r!!1) /= (Jogador 2) = False
  | elem (Jogador 3) r && (r!!1) /= (Jogador 3) = False
  | elem (Jogador 4) r && (r!!1) /= (Jogador 4) = False
  | elem (Presente Patins)  r && (r!!1) /= (Presente Patins) = False
  | elem (Presente Bomba)  r && (r!!1) /= (Presente Bomba) = False
  | elem (Presente Arremesso)  r && (r!!1) /= (Presente Arremesso) = False
  | elem Parede r && (r!!1) /= Parede && (r!!2) /= Parede = False
  | otherwise = True

--O tabuleiro é representado por uma matriZ 9x9
--Os jogadores começarão nos extremos -> Exemplo binário em uma matriz 4x4, onde 1 implica em jogador
{-         [1][0][0][1]
           [0][0][0][0]
           [0][0][0][0]
           [1][0][0][1]          -}


-- A seguir, células para a criação de uma tabuleiro inicial simples:
pedra :: Célula
pedra = [Pedra]  --Matriz[par][par]

m11 :: Célula
m11 = [Grama, Jogador 1]  --Matriz[1][1]

m19 :: Célula
m19 = [Grama, Jogador 2]  --Matriz[1][9]

m91 :: Célula
m91 = [Grama, Jogador 3]  --Matriz[9][1]

m99 :: Célula
m99 = [Grama, Jogador 4]  --Matriz[9][9]

p1 :: Célula
p1 = [Grama, Presente Patins, Parede]  --Jogador pode quebrar parede e encontrar presente

p2 :: Célula
p2 = [Grama, Parede]  --Jogador pode quebrar parede

grama :: Célula
grama = [Grama]

-- Representação tabuleiro no inicio do jogo:
linha1 = [m11,grama,p2,p2,p2,p2,p2,grama,m19]
linha2 = [grama,pedra,p2,pedra,p2,pedra,p1,pedra,grama]
linha3 = [p1,p2,p2,p1,p2,p2,p2,p2,p2]
linha4 = [p2,pedra,p2,pedra,p1,pedra,p2,pedra,p2]
linha5 = [p2,p2,p2,p2,p2,p2,p2,p2,p1]
linha6 = [p2,pedra,p1,pedra,p2,pedra,p2,pedra,p2]
linha7 = [p2,p2,p2,p2,p1,p2,p2,p1,p2]
linha8 = [grama,pedra,p2,pedra,p2,pedra,p2,pedra,grama]
linha9 = [m91,grama,p1,p2,p2,p2,p2,grama,m99]


-- Início do tabuleiro:
tabInicial :: Tabuleiro
tabInicial = [linha1,linha2,linha3,linha4,linha5,linha6,linha7,linha8,linha9]

{-
Para cada jogador, você precisa manter algumas informações extra como:

    identificador (o X que aparece no Item jogador_X)
    localização - é uma tupla com coordenadas X e Y do tipo Int que representam a linha e coluna em o item jogador_X correspondente está.
    direção - é um caractere que indica para onde on jogador está olhando.
        'N', 'S', 'L' e 'O'
    capacidades - é uma tupla com 3 elementos com um dos seguintes valores, onde a, b e c são Int
        (Patins,a)
        (Bomba,b)
        (Arremesso,c)

-}

type Coordenadas = (Int,Int)  -- Coordenadas x e y no tabuleiro
data Direcao = N | S | L | O deriving (Eq, Show)
type Capacidades = [(Elem,Int)]

{-
Criação de um tabuleiro:
    ->Função receba uma lista de listas de items e constrói um tabuleiro válido.
-}

verificaRegras :: Linha -> Bool
verificaRegras p
  | null p = True
  | regras (head p) = verificaRegras f
  | otherwise = False
      where f = tail p

criaTabuleiro :: Tabuleiro -> Tabuleiro
criaTabuleiro p
  | null p = []
  | verificaRegras (head p) = (head p) : criaTabuleiro func
  | otherwise = error "Erro ao criar tabuleiro"
      where func = tail p

{-
Função que receba um tabuleiro e uma instrução de movimentação de um jogador e retorne um novo tabuleiro, com o jogador na nova posição.

  Jogador só pode se deslocar para célula adjacente que não tenha pedra ou bomba
  Pode ser impossível ao jogador se deslocar
  Ao se deslocar para uma célula vazia, cai no buraco
  Ao se deslocar para uma célula com um presente, o coleta
-}

-- Primeiramente, definimos uma função para criar os jogadores, nas posições em que se encontram inicialmente no Tabuleiro
-- com suas respectivas capacidades:
player :: Item -> Player
player p
  | p == Jogador 1 = (1, (0,0), [(Patins,0), (Bomba,0), (Arremesso,0)])
  | p == Jogador 2 = (2, (0,8), [(Patins,0), (Bomba,0), (Arremesso,0)])
  | p == Jogador 3 = (3, (8,0), [(Patins,0), (Bomba,0), (Arremesso,0)])
  | p == Jogador 4 = (4, (8,8), [(Patins,0), (Bomba,0), (Arremesso,0)])
  | otherwise = error "Player não encontrado!"

-- Agora, funções auxiliares caso precisemos pegar os elementos que compõem um jogador:
idPlayer :: Player -> Int
idPlayer (a, _, _) = a

posPlayer :: Player -> (Int, Int)
posPlayer (_, b, _) = b

powPlayer :: Player -> [(Elem, Int)]
powPlayer (_, _, c) = c

mudaCoord :: Player -> Coordenadas -> Player
mudaCoord (x, y, z) a = (x, a, z)

-- Movimetação do jogador
-- --------------------------------------------------

-- Vetores começam em ZERO!
moveVerify :: Player -> Direcao -> Bool
moveVerify jogador dir
  | fst (posPlayer jogador) < 8 && dir == S = True
  | fst (posPlayer jogador) > 0 && dir == N = True
  | snd (posPlayer jogador) > 0 && dir == O = True
  | snd (posPlayer jogador) < 8 && dir == L = True
  | otherwise = False

movePlayer :: Player -> Direcao -> Player
movePlayer jogador dir
  | fst (posPlayer jogador) < 8 && dir == S = mudaCoord jogador f1
  | fst (posPlayer jogador) > 0 && dir == N = mudaCoord jogador f2
  | snd (posPlayer jogador) > 0 && dir == O = mudaCoord jogador f3
  | snd (posPlayer jogador) < 8 && dir == L = mudaCoord jogador f4
  | otherwise = error "Movimentação ilegal!"
                where f1 = (fst (posPlayer jogador) + 1, snd (posPlayer jogador))
                      f2 = (fst (posPlayer jogador) - 1, snd (posPlayer jogador))
                      f3 = (fst (posPlayer jogador), snd (posPlayer jogador) - 1)
                      f4 = (fst (posPlayer jogador), snd (posPlayer jogador) + 1)

tabVerify :: Coordenadas -> Direcao -> Tabuleiro -> Bool
tabVerify pos dir tab
  | dir == N && (((tab!!f1)!!f2) == [Grama]
    || ((tab!!f1)!!f2) == [Grama, Presente Patins]) = True
  | dir == S && (((tab!!f3)!!f2) == [Grama]
    || ((tab!!f3)!!f2) == [Grama, Presente Patins]) = True
  | dir == L && (((tab!!f4)!!f5) == [Grama]
    || ((tab!!f4)!!f5) == [Grama, Presente Patins]) = True
  | dir == O && (((tab!!f4)!!f6) == [Grama]
    || ((tab!!f4)!!f6) == [Grama, Presente Patins]) = True
  | otherwise = False
        where f1 = (fst pos) - 1
              f2 = (snd pos)
              f3 = (fst pos) + 1
              f4 = (fst pos)
              f5 = (snd pos) + 1
              f6 = (snd pos) - 1

players :: [Player]
players = [(1, (0,0), [(Patins,0), (Bomba,0), (Arremesso,0)]),
           (2, (0,8), [(Patins,0), (Bomba,0), (Arremesso,0)]),
           (3, (8,0), [(Patins,0), (Bomba,0), (Arremesso,0)]),
           (4, (8,8), [(Patins,0), (Bomba,0), (Arremesso,0)])]

move :: Player -> Direcao -> Tabuleiro -> Player
move jogador dir tab
  | moveVerify jogador dir && tabVerify (posPlayer jogador) dir tab = movePlayer jogador dir
  | otherwise = error "Deu errado!"

searchPlayer :: [Player] -> Direcao -> Int -> [Player]
searchPlayer jogadores dir num
  | null jogadores =  error "Jogador não encontrado"
  | num == idPlayer (head jogadores) = movePlayer (head jogadores) dir : tail jogadores
  | otherwise = head jogadores : searchPlayer (tail jogadores) dir num

splitPlayer :: [Player] -> Int -> Player
splitPlayer jogadores num
  | idPlayer (head jogadores) == num = head jogadores
  | otherwise = splitPlayer (tail jogadores) num

move' :: Direcao -> Tabuleiro -> [Player] -> Int -> (Tabuleiro, [Player])
move' dir tab jogs num = (novoTab' tab' jogs'' coord , searchPlayer jogs' dir num)
  where jogs' = searchPlayer jogs dir num
        jogs'' = splitPlayer jogs' num 
        tab' = novoTab tab jogs''
        coord = posPlayer jogs''

-- Remover o jogador da célula
removePlayer :: Célula -> Célula
removePlayer c 
  | length c == 1 = []
  | otherwise = head c : removePlayer (tail c)

newLine :: Linha -> Player -> Linha
newLine linha jogador
  | last (head linha) == Jogador (idPlayer jogador) = removePlayer (head linha) : tail linha
  | otherwise = head linha : newLine (tail linha) jogador

checkLine :: Linha -> Player -> Bool
checkLine linha jogador
  | null linha = False
  | last (head linha) == Jogador (idPlayer jogador) = True
  | otherwise = checkLine (tail linha) jogador

novoTab :: Tabuleiro -> Player -> Tabuleiro
novoTab tab jogador
  | checkLine (head tab) jogador = newLine (head tab) jogador : tail tab
  | otherwise = head tab : novoTab (tail tab) jogador

-- (X) Buscar a nova coordenada do jogador (Int, Int)
-- (X) Buscar a nova linha que o jogador vai ficar
-- (X) Buscar a nova célula que o jogador vai ficar
-- (X) Inserir o jogador na nova célula
-- ( ) Inserir célula com jogador na nova linha
-- ( ) Inserir a nova linha no tabuleiro

getNewPosition :: Player -> Direcao -> Coordenadas
getNewPosition p dir = posPlayer (movePlayer p dir)

getLine' :: Tabuleiro -> Coordenadas -> Linha
getLine' tab c = tab !! (fst c)

getNewCel :: Linha -> Coordenadas -> Célula
getNewCel linha c = linha !! (snd c)

insertPlayer :: Célula -> Player -> Célula
insertPlayer cel p
  | null cel = Jogador (idPlayer p) : []
  | otherwise = head cel : insertPlayer (tail cel) p

insertCel :: Linha -> Coordenadas -> Player -> Linha
insertCel linha c jogador 
  | snd c == 0 = insertPlayer (head linha) jogador : tail linha
  | otherwise = head linha : insertCel (tail linha) c' jogador
  where c' = (fst c, snd c - 1)

novoTab' :: Tabuleiro -> Player -> Coordenadas -> Tabuleiro
novoTab' tab jogador c  
  | fst c == 0 = insertCel (head tab) c jogador : tail tab
  | otherwise = head tab : novoTab' (tail tab) jogador c'
  where c' = (fst c - 1, snd c)

-- --------------------------------------------------
-- Mudanças no tabuleiro
-- --------------------------------------------------
--seachFstPos :: Tabuleiro [Player] int













{-
teste = head players

loop :: Tabuleiro -> [Player] -> IO ()
loop tab jogadores =
    let ids = [i | (i,_,_) <- jogadores] in
    do
        number <- getChar
        pos <- getChar
        if fst(head players) == number then
            move teste pos tab
         else
            print $ "(Player,Action)" ++ show (number,pos)


-}












{-
main :: IO ()
main = do
  actionLoop tabuleiro jogadores
  where (tabuleiro,jogadores) = iniciarTabuleiro



--tabuleiroExemplo = [[Grama,Presente,Jogador 1],[Grama],[Grama],[Grama,Presente,Jogador 2]]
jogadorDadosExemplo = [(1, (0,0), [(Patins,0), (Bomba,0), (Arremesso,0)]),
                       (2, (0,8), [(Patins,0), (Bomba,0), (Arremesso,0)])]

iniciarTabuleiro :: (Tabuleiro,[JogadorDados])
iniciarTabuleiro = (tabInicial, jogadorDadosExemplo)

actionLoop :: Tabuleiro -> [Player] -> IO ()
actionLoop tab jogador =
  let ids = [i | (i, _, _) <- jogador] in
  do
      move <- pegaMov ids
      let (j,op) = fromMaybe (-1,NO_OP) move
      print $ "(Jogador,Ação)" ++ show (j,op)
      if op == Sair
      then return ()
      else let (t',js') = case op of
                              ColocarBomba   -> colocarBomba t js j
                              Agir           -> agir t js j
                              Mover d        -> mover d t js j
                              NO_OP          -> (t,js)
                              _              -> (t,js)
           in actionLoop t' js'

-- Tenta movimentar o jogador na direcao especificada.
mover :: Direção -> Tabuleiro -> [JogadorDados] -> Int -> (Tabuleiro, [JogadorDados])
mover d t js j = (t,js)

-- Descobre se alguma ação é possível para o jogador e executa.
agir :: Tabuleiro -> [JogadorDados] -> Int -> (Tabuleiro, [JogadorDados])
agir t js j = (t,js)

-- Verifica se é possível colocar a boma e coloca.
colocarBomba :: Tabuleiro -> [JogadorDados] -> Int -> (Tabuleiro, [JogadorDados])
colocarBomba t js i = (t,js)
-}
-- -------------------------------------
{- DÚVIDAS



-}
