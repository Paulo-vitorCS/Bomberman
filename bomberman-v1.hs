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

data Item = Grama | Pedra | Presente | Parede |
            Jogador_1 | Jogador_2 | Jogador_3 | Jogador_4
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
    -> jogador só pode estar sobre grama [[[],[Grama, presente] [],]]
-}

regras :: [Item] -> Bool
regras r
  | null r = True
  | elem Grama r && (head r) /= Grama = False
  | elem Pedra r && (head r) /= Pedra = False
  | elem Jogador_1 r && (r!!1) /= Jogador_1 = False
  | elem Jogador_2 r && (r!!1) /= Jogador_2 = False
  | elem Jogador_3 r && (r!!1) /= Jogador_3 = False
  | elem Jogador_4 r && (r!!1) /= Jogador_4 = False
  | elem Presente r && (r!!1) /= Presente = False
  | elem Parede r && (r!!1) /= Parede && (r!!2) /= Parede = False
  | otherwise = True

--O tabuleiro é representado por uma matriZ 9x9
--Os jogadores começarão nos extremos -> Exemplo binário em uma matriz 4x4, onde 1 implica em jogador
{-         [1][0][0][1]
           [0][0][0][0]
           [0][0][0][0]
           [1][0][0][1]          -}

pedra :: Célula
pedra = [Pedra]  --Matriz[par][par]

m11 :: Célula
m11 = [Grama, Jogador_1]  --Matriz[1][1]

m19 :: Célula
m19 = [Grama, Jogador_2]  --Matriz[1][9]

m91 :: Célula
m91 = [Grama, Jogador_3]  --Matriz[9][1]

m99 :: Célula
m99 = [Grama, Jogador_4]  --Matriz[9][9]

p1 :: Célula
p1 = [Grama, Presente, Parede]  --Jogador pode quebrar parede e encontrar presente

p2 :: Célula
p2 = [Grama, Parede]  --Jogador pode quebrar parede

grama :: Célula
grama = [Grama]
--Representação tabuleiro no inicio do jogo:
linha1 = [m11,grama,p2,p2,p2,p2,p2,grama,m19]
linha2 = [grama,pedra,p2,pedra,p2,pedra,p1,pedra,grama]
linha3 = [p1,p2,p2,p1,p2,p2,p2,p2,p2]
linha4 = [p2,pedra,p2,pedra,p1,pedra,p2,pedra,p2]
linha5 = [p2,p2,p2,p2,p2,p2,p2,p2,p1]
linha6 = [p2,pedra,p1,pedra,p2,pedra,p2,pedra,p2]
linha7 = [p2,p2,p2,p2,p1,p2,p2,p1,p2]
linha8 = [grama,pedra,p2,pedra,p2,pedra,p2,pedra,grama]
linha9 = [m91,grama,p1,p2,p2,p2,p2,grama,m99]


--Início do tabuleiro:
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
data Elem = Patins | Bomba | Arremesso deriving (Eq, Show)
type Capacidades = ((Elem,Int), (Elem,Int), (Elem,Int))

{-
Criação de um tabuleiro:
    ->Função receba uma lista de listas de items e constrói um tabuleiro válido.
-}

verificaRegras :: [[Item]] -> Bool
verificaRegras p
  | null p = True
  | regras (head p) = verificaRegras f
  | otherwise = False
      where f = tail p

--criaTabuleiro :: [[[Item]]] -> Tabuleiro
criaTabuleiro p
  | null p = []
  | verificaRegras (head p) = (head p) : criaTabuleiro func
  | otherwise = tabInicial-- error "Erro ao criar tabuleiro"
      where func = tail p

{-
Função que receba um tabuleiro e uma instrução de movimentação de um jogador e retorne um novo tabuleiro, com o jogador na nova posição.

  Jogador só pode se deslocar para célula adjacente que não tenha pedra ou bomba
  Pode ser impossível ao jogador se deslocar
  Ao se deslocar para uma célula vazia, cai no buraco
  Ao se deslocar para uma célula com um presente, o coleta
-}

--movimenta :: tabuleiro -> Direcao -> tabuleiro
--movimenta t d


{- DÚVIDAS


-}
