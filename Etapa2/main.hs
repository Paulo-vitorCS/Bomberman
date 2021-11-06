module Main where

import BombermanV3

import Text.Read ( readMaybe )
import Data.Maybe ( fromMaybe, isNothing, fromJust )

data Ação = ColocarBomba | Agir | Mover Direcao | NO_OP | Sair
    deriving (Show, Eq)

keyMaps = [(1,[('e',ColocarBomba),('r',Agir),('a', Mover O),('s', Mover S),('d',Mover L),('w', Mover N),('Q', Sair)]),
           (2,[('o',ColocarBomba),('p',Agir),('j', Mover O),('k', Mover S),('l',Mover L),('i', Mover N),('Q', Sair)])]

mapKey :: Char -> [(Int, [(Char, Ação)])] -> Maybe (Int, Ação)
mapKey c []     = Nothing
mapKey c ((j,as):jas) = case mapKey' c as of Nothing -> mapKey c jas
                                             Just a  -> Just (j,a)
    where mapKey' c [] = Nothing
          mapKey' c ((c',a):ms)
            | c == c'   = Just a
            | otherwise = mapKey' c ms

-- Retorna IO id do jogador e ação a ser executada.
pegaMov :: [Int] -> IO (Maybe (Int,Ação))
pegaMov js = do
        movChar <- getChar
        return (let mapped = mapKey movChar keyMaps
                in case mapped of Nothing     -> Nothing
                                  Just (j,a)  -> if j `elem` js then mapped
                                                                else Nothing)

main :: IO ()
main = do
    actionLoop tabuleiro jogadores
    where (tabuleiro,jogadores) = iniciarTabuleiro

iniciarTabuleiro :: (Tabuleiro,[Player])
iniciarTabuleiro = (tabInicial, players)

actionLoop :: Tabuleiro -> [Player] -> IO ()
actionLoop t js =
    let ids = [i | (i,_,_) <- js] in
    do
        move <- pegaMov ids
        let (j,op) = fromMaybe (-1,NO_OP) move
        print $ "(Player,Action)" ++ show (j,op)
        print $ "(Tabuleiro)" ++ show t
        print $ "(Jogadores)" ++ show js
        if op == Sair
        then return ()
        else let (t',js') = case op of
                                ColocarBomba   -> colocarBomba t js j
                                Agir           -> agir t js j
                                Mover d        -> move' d t js j
                                NO_OP          -> (t,js)
                                _              -> (t,js)
             in actionLoop t' js'

-- Tenta movimentar o jogador na direcao especificada.
mover :: Direcao -> Tabuleiro -> [Player] -> Int -> (Tabuleiro, [Player])
mover d t js j = (t,js)

-- Descobre se alguma ação é possível para o jogador e executa.
agir :: Tabuleiro -> [Player] -> Int -> (Tabuleiro, [Player])
agir t js j = (t,js)

-- Verifica se é possível colocar a boma e coloca.
colocarBomba :: Tabuleiro -> [Player] -> Int -> (Tabuleiro, [Player])
colocarBomba t js i = (t,js)

